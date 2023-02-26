module EndPoints.EditOneCategory
  ( editOneCategory,
    editCategory,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (MonadTrans (lift))
import qualified Control.Monad.Trans.Except as EX
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as SQL
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified EndPoints.Lib.Category.Category as Category
import qualified EndPoints.Lib.Category.CategoryHelpTypes as CategoryHelpTypes
import qualified EndPoints.Lib.Category.CategoryIO as CategoryIO
import qualified EndPoints.Lib.Lib as Lib
import qualified EndPoints.Lib.ToHttpResponse as ToHttpResponse
import qualified EndPoints.Lib.ToText as ToText
import Logger (logDebug, logError, logInfo, (.<))
import qualified News
import Servant (Handler)
import qualified Types.DataTypes as DataTypes
import qualified Types.ErrorTypes as ErrorTypes

editOneCategory ::
  News.Handle IO ->
  DataTypes.Db ->
  DataTypes.User ->
  Int ->
  DataTypes.EditCategoryRequest ->
  Handler DataTypes.Category
editOneCategory h DataTypes.Db {..} user catId r =
  (>>=)
    (liftIO $ dbEditCategory (h, user, catId, r))
    ToHttpResponse.toHttpResponse

editCategory ::
  SQL.Connection ->
  (News.Handle IO, DataTypes.User, Int, DataTypes.EditCategoryRequest) ->
  IO (Either ErrorTypes.AddEditCategoryError DataTypes.Category)
editCategory conn (h, user, catId, r) = EX.runExceptT $ editCategoryExcept conn (h, user, catId, r)

editCategoryExcept ::
  SQL.Connection ->
  (News.Handle IO, DataTypes.User, Int, DataTypes.EditCategoryRequest) ->
  EX.ExceptT ErrorTypes.AddEditCategoryError IO DataTypes.Category
editCategoryExcept conn (h, user, catId, r) = do
  liftIO $ Logger.logInfo (News.hLogHandle h) $ T.concat ["Request: Edit Category: \n", ToText.toText r, "with category id ", T.pack $ show catId, "\nby user: ", ToText.toText user]
  categories <- CategoryIO.getAllCategories conn
  editCategoryFullRequest <- allCheck conn (h, user, catId, r, categories)
  liftIO $ Logger.logDebug (News.hLogHandle h) $ T.concat ["editCategoryExcept:allCheck: OK!  \n", ToText.toText editCategoryFullRequest]
  case Category.changePathsForEditCategory editCategoryFullRequest categories of
    Nothing -> do
      liftIO $ Logger.logError (News.hLogHandle h) ("ERROR " .< ErrorTypes.InvalidValuePath (ErrorTypes.InvalidContent "check Developer Error, then update categories table"))
      EX.throwE $ ErrorTypes.InvalidValuePath $ ErrorTypes.InvalidContent []
    Just toChangePaths -> do
      _ <- CategoryIO.changePathCategories conn h toChangePaths
      editCategoryName conn h editCategoryFullRequest

allCheck ::
  SQL.Connection ->
  (News.Handle IO, DataTypes.User, Int, DataTypes.EditCategoryRequest, [DataTypes.Category]) ->
  EX.ExceptT ErrorTypes.AddEditCategoryError IO CategoryHelpTypes.EditCategoryFullRequest
allCheck conn (h, user, catId, r, categories) = do
  let checkUserAdmin'' = EX.withExceptT ErrorTypes.InvalidPermissionAddEditCategory (Lib.checkUserAdmin h user)
  checkUserAdmin'' >> checkId conn h catId >> checkSyntaxPath h r >> Category.checkLogicPathForEditCategory h catId r categories

-- | checkIdIO  - check if there is a record with the given category id in the database ( id = 7 in http://localhost:8080/category/7 )
checkId ::
  SQL.Connection ->
  News.Handle IO ->
  Int ->
  EX.ExceptT ErrorTypes.AddEditCategoryError IO Int
checkId conn h catId = do
  res <-
    liftIO
      ( SQL.query
          conn
          [sql| SELECT EXISTS (SELECT category_id  FROM category WHERE category_id = ?) |]
          (SQL.Only catId)
      )
  case res of
    [] -> do
      liftIO $ Logger.logError (News.hLogHandle h) ("ERROR " .< ErrorTypes.AddEditCategorySQLRequestError (ErrorTypes.SQLRequestError "checkId! Don't checkId category"))
      EX.throwE $
        ErrorTypes.AddEditCategorySQLRequestError $
          ErrorTypes.SQLRequestError []
    _ ->
      if SQL.fromOnly $ head res
        then
          ( do
              liftIO $ Logger.logDebug (News.hLogHandle h) "checkId: OK! Exists category"
              return catId
          )
        else
          ( do
              liftIO $ Logger.logError (News.hLogHandle h) ("ERROR " .< ErrorTypes.InvalidCategoryId (ErrorTypes.InvalidId "checkId: BAD! Not exists category with id "))
              EX.throwE $ ErrorTypes.InvalidCategoryId $ ErrorTypes.InvalidId []
          )

checkSyntaxPath ::
  Monad m =>
  News.Handle m ->
  DataTypes.EditCategoryRequest ->
  EX.ExceptT ErrorTypes.AddEditCategoryError m DataTypes.EditCategoryRequest
checkSyntaxPath
  _
  r@( DataTypes.EditCategoryRequest
        { DataTypes.newPath = Nothing,
          DataTypes.newCategory = _
        }
      ) = return r
checkSyntaxPath
  h
  r@( DataTypes.EditCategoryRequest
        { DataTypes.newPath = Just path,
          DataTypes.newCategory = _
        }
      ) =
    if Category.validSyntaxPath path
      then return r
      else do
        lift $ Logger.logError (News.hLogHandle h) ("ERROR " .< ErrorTypes.InvalidSyntaxPath (ErrorTypes.InvalidContent "checkSyntaxPath: BAD! Path is not valid! Only digits(not zero begin) and points must have! "))
        EX.throwE $ ErrorTypes.InvalidSyntaxPath $ ErrorTypes.InvalidContent []

editCategoryName ::
  SQL.Connection ->
  News.Handle IO ->
  CategoryHelpTypes.EditCategoryFullRequest ->
  EX.ExceptT ErrorTypes.AddEditCategoryError IO DataTypes.Category
editCategoryName conn h CategoryHelpTypes.EditCategoryFullRequest {..} = do
  res <-
    liftIO $
      SQL.execute
        conn
        [sql| UPDATE category SET category_name = ? WHERE category_id = ? |]
        (newCategory', id')
  case read (show res) :: Int of
    1 -> do
      rez_new_path <-
        liftIO $
          SQL.query
            conn
            [sql| SELECT category_path FROM category WHERE category_id = ? |]
            (SQL.Only id')
      let rez_new_path' = SQL.fromOnly . head $ rez_new_path :: String
      let editedCategory =
            Category.toCategories (rez_new_path', id', newCategory')
      liftIO $ Logger.logInfo (News.hLogHandle h) $ T.concat ["editCategoryIO: OK! UPDATE  category: ", ToText.toText editedCategory]

      return editedCategory
    _ -> do
      liftIO $ Logger.logError (News.hLogHandle h) ("ERROR " .< ErrorTypes.AddEditCategorySQLRequestError (ErrorTypes.SQLRequestError "editCategoryIO: BAD! Don't UPDATE  category"))
      EX.throwE $
        ErrorTypes.AddEditCategorySQLRequestError $
          ErrorTypes.SQLRequestError []
