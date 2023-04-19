module EndPoints.Lib.Category.CategoryIO
  ( getCategoriesById,
    getCategoryById,
    checkCategoryExistsById,
  )
where

import qualified Control.Exception.Safe as EXS
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Control.Monad.Trans.Except as EX
import qualified Data.Pool as POOL
import qualified Database.PostgreSQL.Simple as SQL
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified EndPoints.Lib.ThrowRequestError as Throw
import qualified EndPoints.Lib.ToText as ToText
import Logger (logDebug, logInfo)
import qualified News
import qualified Types.DataTypes as DataTypes

-- | getCategoriesById  - return categories from node to root, by categoryId
getCategoriesById ::
  Throw.ThrowSqlRequestError a [DataTypes.Category] =>
  POOL.Pool SQL.Connection ->
  News.Handle IO ->
  DataTypes.Id DataTypes.CategoryId ->
  EX.ExceptT a IO [DataTypes.Category]
getCategoriesById pool h' id' = do
  res <-
    liftIO
      ( EXS.try
          ( POOL.withResource pool $ \con ->
              SQL.query
                con
                [sql| WITH RECURSIVE temp1 (category_id, category_parent_id,category_name, path) AS (
                      SELECT t1.category_id, t1.category_parent_id, t1.category_name, CAST (t1.category_name AS varchar (300)) AS path
                      FROM category t1 WHERE t1.category_id = ?
                      UNION
                      SELECT t2.category_id, t2.category_parent_id, t2.category_name, CAST (temp1.path || '->'|| t2.category_name AS varchar(300))
                      FROM category t2 INNER JOIN temp1 ON (temp1.category_parent_id = t2.category_id))
                      SELECT category_id,  category_name, category_parent_id from temp1 |]
                (SQL.Only $ DataTypes.getId id')
          ) ::
          IO (Either EXS.SomeException [(DataTypes.Id DataTypes.CategoryId, DataTypes.Name, DataTypes.Id DataTypes.CategoryId)])
      )
  case res of
    Left err -> Throw.throwSqlRequestError h' ("getCategoriesById", show err)
    Right [] -> Throw.throwSqlRequestError h' ("getCategoriesById", "Developer error")
    Right value -> do
      let categories = Prelude.map (\(a, b, c) -> DataTypes.Category a b c) value
      return categories

-- | getCategoriesById  - return category by categoryId
getCategoryById ::
  Throw.ThrowSqlRequestError a DataTypes.Category =>
  POOL.Pool SQL.Connection ->
  News.Handle IO ->
  DataTypes.Id DataTypes.CategoryId ->
  EX.ExceptT a IO DataTypes.Category
getCategoryById pool h id' = do
  res <-
    liftIO
      ( EXS.try
          ( POOL.withResource pool $ \conn ->
              SQL.query
                conn
                [sql| SELECT category_id, category_name, category_parent_id FROM category WHERE category_id = ? |]
                (SQL.Only $ DataTypes.getId id')
          ) ::
          IO (Either EXS.SomeException [(DataTypes.Id DataTypes.CategoryId, DataTypes.Name, DataTypes.Id DataTypes.CategoryId)])
      )
  case res of
    Left err -> Throw.throwSqlRequestError h ("getCategory", show err)
    Right [(idCat, name, parent)] -> do
      let category = DataTypes.Category idCat name parent
      liftIO $ Logger.logInfo (News.hLogHandle h) $ "getCategory: " <> ToText.toText category
      return category
    Right _ -> Throw.throwSqlRequestError h ("getCategory", "Developer error")

-- | checkCategoryExistsById  - return categories id if category exists
checkCategoryExistsById ::
  Throw.ThrowSqlRequestError a (DataTypes.Id DataTypes.CategoryId) =>
  Throw.ThrowInvalidContentCategoryId a (DataTypes.Id DataTypes.CategoryId) =>
  POOL.Pool SQL.Connection ->
  News.Handle IO ->
  DataTypes.Id DataTypes.CategoryId ->
  EX.ExceptT a IO (DataTypes.Id DataTypes.CategoryId)
checkCategoryExistsById pool h categoryId = do
  res <-
    liftIO
      ( EXS.try
          ( POOL.withResource pool $ \conn ->
              SQL.query
                conn
                [sql| SELECT EXISTS (SELECT category_id  FROM category WHERE category_id = ?) |]
                (SQL.Only $ DataTypes.getId categoryId)
          ) ::
          IO (Either EXS.SomeException [SQL.Only Bool])
      )
  case res of
    Left err -> Throw.throwSqlRequestError h ("checkCategoryExistsById", show err)
    Right [SQL.Only True] -> do
      liftIO $ Logger.logDebug (News.hLogHandle h) "checkCategoryExistsById: OK!  Category exists "
      return categoryId
    Right [SQL.Only False] -> Throw.throwInvalidContentCategoryId h ("checkCategoryExistsById", "Category with categoryId " <> show categoryId <> " not exists")
    Right _ -> Throw.throwSqlRequestError h ("checkCategoryId", "Developer error")
