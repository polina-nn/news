module EndPoints.AddOneCategory
  ( addOneCategory,
    addCategory,
  )
where

import qualified Control.Exception.Safe as EXS
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Control.Monad.Trans.Except as EX
import qualified Data.Pool as POOL
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as SQL
import Database.PostgreSQL.Simple.SqlQQ (sql)
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

addOneCategory ::
  News.Handle IO ->
  DataTypes.Db ->
  DataTypes.Token ->
  DataTypes.CreateCategoryRequest ->
  Handler DataTypes.Category
addOneCategory h DataTypes.Db {..} user createCategoryReq =
  (>>=)
    (liftIO $ dbAddCategory (h, user, createCategoryReq))
    ToHttpResponse.toHttpResponse

addCategory ::
  POOL.Pool SQL.Connection ->
  (News.Handle IO, DataTypes.Token, DataTypes.CreateCategoryRequest) ->
  IO (Either ErrorTypes.AddEditCategoryError DataTypes.Category)
addCategory pool (h, token, r) = EX.runExceptT $ addCategoryExcept pool (h, token, r)

addCategoryExcept ::
  POOL.Pool SQL.Connection ->
  (News.Handle IO, DataTypes.Token, DataTypes.CreateCategoryRequest) ->
  EX.ExceptT ErrorTypes.AddEditCategoryError IO DataTypes.Category
addCategoryExcept pool (h, token, r) =
  do
    user <- EX.withExceptT ErrorTypes.AddEditCategorySQLRequestError (LibIO.searchUser h pool token)
    liftIO $ Logger.logInfo (News.hLogHandle h) $ T.concat ["\n\nRequest: Add Category: \n", ToText.toText r, "by user: ", ToText.toText user]
    _ <- EX.withExceptT ErrorTypes.InvalidPermissionAddEditCategory (Lib.checkUserAdmin h user)
    _ <- checkCategoryName pool h r
    _ <- checkParentId pool h r
    addCategoryToDb pool h r

-- | checkCategoryName - check the existence of the category with the same name. Duplication of category name is not allowed
checkCategoryName ::
  POOL.Pool SQL.Connection ->
  News.Handle IO ->
  DataTypes.CreateCategoryRequest ->
  EX.ExceptT ErrorTypes.AddEditCategoryError IO DataTypes.CreateCategoryRequest
checkCategoryName pool h' r@DataTypes.CreateCategoryRequest {..} = do
  res <-
    liftIO
      ( EXS.try
          ( POOL.withResource pool $ \conn ->
              SQL.query
                conn
                [sql| SELECT EXISTS (SELECT category_name  FROM category WHERE category_name = ?) |]
                (SQL.Only category)
          ) ::
          IO (Either EXS.SomeException [SQL.Only Bool])
      )
  case res of
    Left err -> Throw.throwSqlRequestError h' ("checkCategoryName", show err)
    Right [SQL.Only True] -> do
      liftIO $
        Logger.logError
          (News.hLogHandle h')
          ("ERROR " .< ErrorTypes.CategoryAlreadyExisted (ErrorTypes.InvalidContent "checkCategoryName: BAD! Category with this name already exists "))
      EX.throwE $ ErrorTypes.CategoryAlreadyExisted $ ErrorTypes.InvalidContent []
    Right [SQL.Only False] -> do
      liftIO $ Logger.logDebug (News.hLogHandle h') "checkCategoryName: OK!"
      return r
    Right _ -> Throw.throwSqlRequestError h' ("checkCategoryName", "Developer error!")

-- | checkParentId  - check the existence of the parent category
checkParentId ::
  POOL.Pool SQL.Connection ->
  News.Handle IO ->
  DataTypes.CreateCategoryRequest ->
  EX.ExceptT ErrorTypes.AddEditCategoryError IO DataTypes.CreateCategoryRequest
checkParentId _ _ r@DataTypes.CreateCategoryRequest {parent = 0} = return r
checkParentId pool h' r@DataTypes.CreateCategoryRequest {..} = do
  res <-
    liftIO
      ( EXS.try
          ( POOL.withResource pool $ \conn ->
              SQL.query
                conn
                [sql| SELECT EXISTS (SELECT category_id  FROM category WHERE category_id = ?) |]
                (SQL.Only parent)
          ) ::
          IO (Either EXS.SomeException [SQL.Only Bool])
      )
  case res of
    Left err -> Throw.throwSqlRequestError h' ("checkParentId", show err)
    Right [SQL.Only False] -> do
      liftIO $
        Logger.logError
          (News.hLogHandle h')
          ("ERROR " .< ErrorTypes.CategoryParentNotExisted (ErrorTypes.InvalidContent "checkParentId: BAD! Category with this id not exists"))
      EX.throwE $ ErrorTypes.CategoryParentNotExisted $ ErrorTypes.InvalidContent []
    Right [SQL.Only True] -> do
      liftIO $ Logger.logDebug (News.hLogHandle h') "checkParentId: OK!"
      return r
    Right _ -> Throw.throwSqlRequestError h' ("checkParentId", "Developer error!")

addCategoryToDb ::
  POOL.Pool SQL.Connection ->
  News.Handle IO ->
  DataTypes.CreateCategoryRequest ->
  EX.ExceptT ErrorTypes.AddEditCategoryError IO DataTypes.Category
addCategoryToDb pool h DataTypes.CreateCategoryRequest {..} = do
  res <-
    liftIO
      ( EXS.try
          ( POOL.withResource pool $ \conn ->
              SQL.query
                conn
                [sql| INSERT INTO  category (category_parent_id, category_name)  VALUES (?,?) RETURNING category_id  |]
                (parent, category)
          ) ::
          IO (Either EXS.SomeException [SQL.Only Int])
      )
  case res of
    Left err -> Throw.throwSqlRequestError h ("addCategoryToDb", show err)
    Right [SQL.Only val] -> do
      let newCategory =
            ( DataTypes.Category
                { categoryParentId = parent,
                  categoryId = val,
                  categoryName = category
                }
            )
      liftIO $ Logger.logInfo (News.hLogHandle h) $ T.concat ["addCategoryToDb: added new category: ", ToText.toText newCategory]
      return newCategory
    Right _ -> Throw.throwSqlRequestError h ("addCategoryToDb", "Developer error")
