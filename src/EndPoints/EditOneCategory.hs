module EndPoints.EditOneCategory
  ( editOneCategory,
    editCategory,
  )
where

import qualified Control.Exception.Safe as EXS
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (MonadTrans (lift))
import qualified Control.Monad.Trans.Except as EX
import qualified Data.Int as I
import qualified Data.Pool as POOL
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as SQL
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified EndPoints.Lib.Category.Category as Category
import qualified EndPoints.Lib.Category.CategoryHelpTypes as CategoryHelpTypes
import qualified EndPoints.Lib.Category.CategoryIO as CategoryIO
import qualified EndPoints.Lib.Lib as Lib
import qualified EndPoints.Lib.LibIO as LibIO
import qualified EndPoints.Lib.ThrowSqlRequestError as Throw
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
  DataTypes.Token ->
  Int ->
  DataTypes.EditCategoryRequest ->
  Handler DataTypes.Category
editOneCategory h DataTypes.Db {..} user catId r =
  (>>=)
    (liftIO $ dbEditCategory (h, user, catId, r))
    ToHttpResponse.toHttpResponse

editCategory ::
  POOL.Pool SQL.Connection ->
  (News.Handle IO, DataTypes.Token, Int, DataTypes.EditCategoryRequest) ->
  IO (Either ErrorTypes.AddEditCategoryError DataTypes.Category)
editCategory pool (h, token, catId, r) = EX.runExceptT $ editCategoryExcept pool (h, token, catId, r)

editCategoryExcept ::
  POOL.Pool SQL.Connection ->
  (News.Handle IO, DataTypes.Token, Int, DataTypes.EditCategoryRequest) ->
  EX.ExceptT ErrorTypes.AddEditCategoryError IO DataTypes.Category
editCategoryExcept pool (h, token, catId, r) = do
  _ <- checkId pool h catId
  user <- EX.withExceptT ErrorTypes.AddEditCategorySQLRequestError (LibIO.searchUser h pool token)
  liftIO $ Logger.logInfo (News.hLogHandle h) $ T.concat ["Request: Edit Category: \n", ToText.toText r, "with category id ", T.pack $ show catId, "\nby user: ", ToText.toText user]
  _ <- EX.withExceptT ErrorTypes.InvalidPermissionAddEditCategory (Lib.checkUserAdmin h user)
  _ <- checkSyntaxPath h r
  categories <- CategoryIO.getAllCategories pool h
  editCategoryFullRequest <- Category.checkLogicPathForEditCategory h catId r categories
  liftIO $ Logger.logDebug (News.hLogHandle h) $ T.concat ["editCategoryExcept: allCheck: OK!  \n", ToText.toText editCategoryFullRequest]
  case Category.changePathsForEditCategory editCategoryFullRequest categories of
    Nothing -> do
      liftIO $ Logger.logError (News.hLogHandle h) ("ERROR " .< ErrorTypes.InvalidValuePath (ErrorTypes.InvalidContent "check Developer Error, then update categories table"))
      EX.throwE $ ErrorTypes.InvalidValuePath $ ErrorTypes.InvalidContent []
    Just toChangePaths -> do
      _ <- CategoryIO.changePathCategories pool h toChangePaths
      _ <- editCategoryName pool h editCategoryFullRequest
      getCategory pool h editCategoryFullRequest

-- | checkIdIO  - check if there is a record with the given category id in the database ( id = 7 in http://localhost:8080/category/7 )
checkId ::
  POOL.Pool SQL.Connection ->
  News.Handle IO ->
  Int ->
  EX.ExceptT ErrorTypes.AddEditCategoryError IO Int
checkId pool h' id' = do
  res <-
    liftIO
      ( EXS.try
          ( POOL.withResource pool $ \conn ->
              SQL.query
                conn
                [sql| SELECT EXISTS (SELECT category_id  FROM category WHERE category_id = ?) |]
                (SQL.Only id')
          ) ::
          IO (Either EXS.SomeException [SQL.Only Bool])
      )
  case res of
    Left err -> Throw.throwSqlRequestError h' ("checkId", show err)
    Right [SQL.Only True] -> do
      liftIO $ Logger.logDebug (News.hLogHandle h') "checkId: OK!  Category exist "
      return id'
    Right [SQL.Only False] -> do
      liftIO $ Logger.logError (News.hLogHandle h') ("ERROR " .< ErrorTypes.InvalidImagedId (ErrorTypes.InvalidId ("checkId: BAD! Not exists category with id " <> show id')))
      EX.throwE $ ErrorTypes.InvalidCategoryId $ ErrorTypes.InvalidId []
    Right _ -> Throw.throwSqlRequestError h' ("checkId", "Developer error")

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
  POOL.Pool SQL.Connection ->
  News.Handle IO ->
  CategoryHelpTypes.EditCategoryFullRequest ->
  EX.ExceptT ErrorTypes.AddEditCategoryError IO CategoryHelpTypes.EditCategoryFullRequest
editCategoryName pool h r@CategoryHelpTypes.EditCategoryFullRequest {..} = do
  res <-
    liftIO
      ( EXS.try
          ( POOL.withResource pool $ \conn ->
              SQL.execute
                conn
                [sql| UPDATE category SET category_name = ? WHERE category_id = ? |]
                (newCategory', id')
          ) ::
          IO (Either EXS.SomeException I.Int64)
      )
  case res of
    Left err -> Throw.throwSqlRequestError h ("editCategoryName", show err)
    Right 1 -> return r
    Right _ -> Throw.throwSqlRequestError h ("editCategoryName", "Developer error")

getCategory ::
  POOL.Pool SQL.Connection ->
  News.Handle IO ->
  CategoryHelpTypes.EditCategoryFullRequest ->
  EX.ExceptT ErrorTypes.AddEditCategoryError IO DataTypes.Category
getCategory pool h CategoryHelpTypes.EditCategoryFullRequest {..} = do
  resPath <-
    liftIO
      ( EXS.try
          ( POOL.withResource pool $ \conn ->
              SQL.query
                conn
                [sql| SELECT category_path FROM category WHERE category_id = ? |]
                (SQL.Only id')
          ) ::
          IO (Either EXS.SomeException [SQL.Only String])
      )
  case resPath of
    Left err -> Throw.throwSqlRequestError h ("getCategory", show err)
    Right [SQL.Only val] -> do
      let editedCategory = Category.toCategories (val, id', newCategory')
      liftIO $ Logger.logInfo (News.hLogHandle h) $ T.concat ["UPDATE  category: ", ToText.toText editedCategory]
      return editedCategory
    Right _ -> Throw.throwSqlRequestError h ("getCategory", "Developer error")
