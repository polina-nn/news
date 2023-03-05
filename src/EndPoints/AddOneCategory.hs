module EndPoints.AddOneCategory
  ( addOneCategory,
    addCategory,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (MonadTrans (lift))
import qualified Control.Monad.Trans.Except as EX
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as SQL
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified EndPoints.Lib.Category.Category as Category
import qualified EndPoints.Lib.Category.CategoryIO as CategoryIO
import qualified EndPoints.Lib.Lib as Lib
import qualified EndPoints.Lib.ToHttpResponse as ToHttpResponse
import qualified EndPoints.Lib.ToText as ToText
import Logger (logError, logInfo, (.<))
import qualified News
import Servant (Handler)
import qualified Types.DataTypes as DataTypes
import qualified Types.ErrorTypes as ErrorTypes

addOneCategory ::
  News.Handle IO ->
  DataTypes.Db ->
  DataTypes.Account ->
  DataTypes.CreateCategoryRequest ->
  Handler DataTypes.Category
addOneCategory h DataTypes.Db {..} user createCategoryReq =
  (>>=)
    (liftIO $ dbAddCategory (h, user, createCategoryReq))
    ToHttpResponse.toHttpResponse

addCategory ::
  SQL.Connection ->
  (News.Handle IO, DataTypes.Account, DataTypes.CreateCategoryRequest) ->
  IO (Either ErrorTypes.AddEditCategoryError DataTypes.Category)
addCategory conn (h, user, r) = EX.runExceptT $ addCategoryExcept conn (h, user, r)

addCategoryExcept ::
  SQL.Connection ->
  (News.Handle IO, DataTypes.Account, DataTypes.CreateCategoryRequest) ->
  EX.ExceptT ErrorTypes.AddEditCategoryError IO DataTypes.Category
addCategoryExcept conn (h, user, r) = undefined

{--
  do
    liftIO $ Logger.logInfo (News.hLogHandle h) $ T.concat ["Request: Add Category: \n", ToText.toText r, "by user: ", ToText.toText user]
    _ <- EX.withExceptT ErrorTypes.InvalidPermissionAddEditCategory (Lib.checkUserAdmin h user)
    _ <- checkSyntaxPath h r
    categories <- CategoryIO.getAllCategories conn
    _ <- Category.checkLogicPathForAddCategory h r categories
    _ <- CategoryIO.changePathCategories conn h $ Category.changePathForAddCategory r categories
    addCategoryToDb conn h r
--}
checkSyntaxPath ::
  Monad m =>
  News.Handle m ->
  DataTypes.CreateCategoryRequest ->
  EX.ExceptT ErrorTypes.AddEditCategoryError m DataTypes.CreateCategoryRequest
checkSyntaxPath h r@DataTypes.CreateCategoryRequest {..} =
  if Category.validSyntaxPath path
    then return r
    else do
      lift $
        Logger.logError
          (News.hLogHandle h)
          ( "ERROR "
              .< ErrorTypes.InvalidSyntaxPath
                ( ErrorTypes.InvalidContent
                    "checkSyntaxPath: BAD! Path is not valid! Only digits(not zero begin) and points must have! "
                )
          )
      EX.throwE $ ErrorTypes.InvalidSyntaxPath $ ErrorTypes.InvalidContent []

addCategoryToDb ::
  SQL.Connection ->
  News.Handle IO ->
  DataTypes.CreateCategoryRequest ->
  EX.ExceptT ErrorTypes.AddEditCategoryError IO DataTypes.Category
addCategoryToDb conn h DataTypes.CreateCategoryRequest {..} = do
  res <-
    liftIO
      ( SQL.execute
          conn
          [sql| INSERT INTO  category (category_path, category_name)  VALUES (?,?) |]
          (path, category)
      )
  case read (show res) :: Int of
    1 -> do
      resId <-
        liftIO
          ( SQL.query
              conn
              [sql| SELECT category_id  FROM category WHERE category_path = ? |]
              (SQL.Only path)
          )
      case resId of
        [val] -> do
          let rez = SQL.fromOnly val
              newCategory =
                ( DataTypes.Category
                    { categoryPath = path,
                      categoryId = rez,
                      categoryName = category
                    }
                )
          liftIO $ Logger.logInfo (News.hLogHandle h) $ T.concat ["addCategoryToDb: OK!", ToText.toText newCategory]
          return newCategory
        _ -> sqlRequestError
    _ -> sqlRequestError
  where
    sqlRequestError :: EX.ExceptT ErrorTypes.AddEditCategoryError IO DataTypes.Category
    sqlRequestError = do
      liftIO $
        Logger.logError
          (News.hLogHandle h)
          ( "ERROR "
              .< ErrorTypes.AddEditCategorySQLRequestError
                ( ErrorTypes.SQLRequestError "addCategoryToDb! Don't INSERT INTO  category"
                )
          )
      EX.throwE $
        ErrorTypes.AddEditCategorySQLRequestError $
          ErrorTypes.SQLRequestError []
