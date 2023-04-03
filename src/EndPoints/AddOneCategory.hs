module EndPoints.AddOneCategory
  ( addOneCategory,
    addCategory,
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
import qualified EndPoints.Lib.Category.CategoryIO as CategoryIO
import qualified EndPoints.Lib.Lib as Lib
import qualified EndPoints.Lib.LibIO as LibIO
import qualified EndPoints.Lib.ThrowSqlRequestError as Throw
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
    liftIO $ Logger.logInfo (News.hLogHandle h) $ T.concat ["Request: Add Category: \n", ToText.toText r, "by user: ", ToText.toText user]
    _ <- EX.withExceptT ErrorTypes.InvalidPermissionAddEditCategory (Lib.checkUserAdmin h user)
    _ <- checkSyntaxPath h r
    categories <- CategoryIO.getAllCategories pool h
    _ <- Category.checkLogicPathForAddCategory h r categories
    _ <- CategoryIO.changePathCategories pool h $ Category.changePathForAddCategory r categories
    _ <- addCategoryToDb pool h r
    getCategoryId pool h r

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
  POOL.Pool SQL.Connection ->
  News.Handle IO ->
  DataTypes.CreateCategoryRequest ->
  EX.ExceptT ErrorTypes.AddEditCategoryError IO DataTypes.CreateCategoryRequest
addCategoryToDb pool h r@DataTypes.CreateCategoryRequest {..} = do
  res <-
    liftIO
      ( EXS.try
          ( POOL.withResource pool $ \conn ->
              SQL.execute
                conn
                [sql| INSERT INTO  category (category_path, category_name)  VALUES (?,?) |]
                (path, category)
          ) ::
          IO (Either EXS.SomeException I.Int64)
      )
  case res of
    Left err -> Throw.throwSqlRequestError h ("addCategoryToDb", show err)
    Right 1 -> return r
    Right _ -> Throw.throwSqlRequestError h ("addCategoryToDb", "Developer error")

getCategoryId ::
  POOL.Pool SQL.Connection ->
  News.Handle IO ->
  DataTypes.CreateCategoryRequest ->
  EX.ExceptT ErrorTypes.AddEditCategoryError IO DataTypes.Category
getCategoryId pool h DataTypes.CreateCategoryRequest {..} = do
  resId <-
    liftIO
      ( EXS.try
          ( POOL.withResource pool $ \conn ->
              SQL.query
                conn
                [sql| SELECT category_id  FROM category WHERE category_path = ? |]
                (SQL.Only path)
          ) ::
          IO (Either EXS.SomeException [SQL.Only Int])
      )
  case resId of
    Left err -> Throw.throwSqlRequestError h ("getCategoryId", show err)
    Right [SQL.Only val] ->
      do
        let newCategory =
              ( DataTypes.Category
                  { categoryPath = path,
                    categoryId = val,
                    categoryName = category
                  }
              )
        liftIO $ Logger.logInfo (News.hLogHandle h) $ T.concat ["Added new category: ", ToText.toText newCategory]
        return newCategory
    Right _ -> Throw.throwSqlRequestError h ("getCategoryId", "Developer error")
