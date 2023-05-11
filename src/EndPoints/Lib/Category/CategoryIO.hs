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
import qualified EndPoints.Lib.ThrowError as Throw
import qualified EndPoints.Lib.ToText as ToText
import Logger (logInfo)
import qualified News
import qualified Types.DataTypes as DataTypes
import qualified Types.ErrorTypes as ErrorTypes

-- | getCategoriesById  - return categories from node to root, by categoryId
getCategoriesById ::
  POOL.Pool SQL.Connection ->
  News.Handle IO ->
  DataTypes.Id DataTypes.Category ->
  EX.ExceptT ErrorTypes.InvalidContentCategoryId IO [DataTypes.Category]
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
                (SQL.Only id')
          ) ::
          IO (Either EXS.SomeException [(DataTypes.Id DataTypes.Category, DataTypes.Name, DataTypes.Id DataTypes.Category)])
      )
  case res of
    Left err -> Throw.throwSomeException h' "getCategoriesById" err
    Right [] -> Throw.throwNotExists h' "getCategoriesById" $ ErrorTypes.InvalidContent " Category not exists"
    Right value -> do
      let categories = Prelude.map (\(a, b, c) -> DataTypes.Category a b c) value
      return categories

-- | getCategoryById  - return category by categoryId
getCategoryById ::
  POOL.Pool SQL.Connection ->
  News.Handle IO ->
  DataTypes.Id DataTypes.Category ->
  EX.ExceptT ErrorTypes.InvalidContentCategoryId IO DataTypes.Category
getCategoryById pool h id' = do
  res <-
    liftIO
      ( EXS.try
          ( POOL.withResource pool $ \conn ->
              SQL.query
                conn
                [sql| SELECT category_id, category_name, category_parent_id FROM category WHERE category_id = ? |]
                (SQL.Only id')
          ) ::
          IO (Either EXS.SomeException [(DataTypes.Id DataTypes.Category, DataTypes.Name, DataTypes.Id DataTypes.Category)])
      )
  case res of
    Left err -> Throw.throwSomeException h "getCategoryById" err
    Right [(idCat, name, parent)] -> do
      let category = DataTypes.Category idCat name parent
      liftIO $ Logger.logInfo (News.hLogHandle h) $ "getCategoryById: " <> ToText.toText category
      return category
    Right [] -> Throw.throwNotExists h "getCategoryById" $ ErrorTypes.InvalidContent " Category not exists"
    Right _ -> Throw.throwSqlRequestError h "getCategoryById" $ ErrorTypes.SQLRequestError " Developer error!"

-- | checkCategoryExistsById  - return categories id if category exists
checkCategoryExistsById ::
  POOL.Pool SQL.Connection ->
  News.Handle IO ->
  DataTypes.Id DataTypes.Category ->
  EX.ExceptT ErrorTypes.InvalidContentCategoryId IO (DataTypes.Id DataTypes.Category)
checkCategoryExistsById pool h categoryId = do
  res <-
    liftIO
      ( EXS.try
          ( POOL.withResource pool $ \conn ->
              SQL.query
                conn
                [sql| SELECT EXISTS (SELECT category_id  FROM category WHERE category_id = ?) |]
                (SQL.Only categoryId)
          ) ::
          IO (Either EXS.SomeException [SQL.Only Bool])
      )
  case res of
    Left err -> Throw.throwSomeException h "checkCategoryExistsById" err
    Right [SQL.Only True] -> return categoryId
    Right [SQL.Only False] -> Throw.throwNotExists h "checkCategoryExistsById" $ ErrorTypes.InvalidContent " Category not exists"
    Right _ -> Throw.throwSqlRequestError h "checkCategoryExistsById" $ ErrorTypes.SQLRequestError " Developer error!"
