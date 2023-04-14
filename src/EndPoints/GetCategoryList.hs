module EndPoints.GetCategoryList
  ( getCategoryList,
    categoryList,
  )
where

import qualified Control.Exception.Safe as EXS
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Control.Monad.Trans.Except as EX
import Data.List (sortBy)
import qualified Data.Pool as POOL
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as SQL
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified EndPoints.Lib.Category.Category as Category
import qualified EndPoints.Lib.Category.CategoryHelpTypes as CategoryHelpTypes
import qualified EndPoints.Lib.OffsetLimit as OffsetLimit
import qualified EndPoints.Lib.ThrowRequestError as Throw
import qualified EndPoints.Lib.ToHttpResponse as ToHttpResponse
import qualified EndPoints.Lib.ToText as ToText
import Logger (logDebug, logInfo, (.<))
import qualified News
import Servant (Handler)
import qualified Types.DataTypes as DataTypes
import qualified Types.ErrorTypes as ErrorTypes

getCategoryList ::
  News.Handle IO ->
  DataTypes.Db ->
  Maybe DataTypes.Offset ->
  Maybe DataTypes.Limit ->
  Handler [DataTypes.Category]
getCategoryList h DataTypes.Db {..} ma ml =
  (>>=) (liftIO $ dbCategoryList (h, ma, ml)) ToHttpResponse.toHttpResponse

categoryList ::
  POOL.Pool SQL.Connection ->
  (News.Handle IO, Maybe DataTypes.Offset, Maybe DataTypes.Limit) ->
  IO (Either ErrorTypes.GetContentError [DataTypes.Category])
categoryList pool (h, mo, ml) = do EX.runExceptT $ categoryListExcept pool (h, mo, ml)

categoryListExcept ::
  POOL.Pool SQL.Connection ->
  (News.Handle IO, Maybe DataTypes.Offset, Maybe DataTypes.Limit) ->
  EX.ExceptT ErrorTypes.GetContentError IO [DataTypes.Category]
categoryListExcept pool (h, mo, ml) = do
  liftIO $ Logger.logInfo (News.hLogHandle h) $ "\n\nRequest: Get Category List  with offset = " .< mo <> " limit = " .< ml
  (offset, limit) <- EX.withExceptT ErrorTypes.InvalidOffsetOrLimitGetContent $ OffsetLimit.checkOffsetLimit h mo ml
  getAllCategories pool h offset limit

getAllCategories ::
  POOL.Pool SQL.Connection ->
  News.Handle IO ->
  DataTypes.Offset ->
  DataTypes.Limit ->
  EX.ExceptT ErrorTypes.GetContentError IO [DataTypes.Category]
getAllCategories pool h offset limit = do
  res <-
    liftIO
      ( EXS.try
          ( POOL.withResource pool $ \conn ->
              SQL.query_
                conn
                [sql| WITH RECURSIVE tree AS (
                      SELECT category_id, category_name, category_parent_id, category_name  AS sort_string, 1 AS depth
                      FROM category
                      WHERE category_parent_id = 0 
                      UNION ALL
                      SELECT s1.category_id, s1.category_name, s1.category_parent_id, tree.sort_string || '|' || s1.category_name AS sort_string, tree.depth+1 AS depth
                      FROM  tree
                      JOIN category s1 ON s1.category_parent_id = tree.category_id )
                      SELECT   category_id, category_name, category_parent_id, sort_string FROM tree ORDER BY sort_string ASC; |]
          ) ::
          IO (Either EXS.SomeException [(Int, DataTypes.Name, Int, DataTypes.Name)])
      )
  case res of
    Left err -> Throw.throwSqlRequestError h ("getAllCategories", show err)
    Right value -> do
      let categorySort = Prelude.map Category.toCategorySort value
          result = getCategoriesWithOffsetLimit offset limit (sortedAllCategories categorySort)
          toTextCategories = T.concat $ map ToText.toText result
      liftIO $ Logger.logDebug (News.hLogHandle h) $ "categories: OK! \n" <> toTextCategories
      return result

-- | sortedAllCategories - postgreSQL returns an alphabetically sorted result for Latin only, Cyrillic has to be sorted by Haskell
sortedAllCategories :: [CategoryHelpTypes.CategorySort] -> [DataTypes.Category]
sortedAllCategories cats = Prelude.map Category.toCategoryFromCategorySort rez
  where
    rez =
      sortBy
        ( \x y ->
            compare
              (CategoryHelpTypes.categorySortPath x)
              (CategoryHelpTypes.categorySortPath y)
        )
        cats

getCategoriesWithOffsetLimit :: DataTypes.Offset -> DataTypes.Limit -> [DataTypes.Category] -> [DataTypes.Category]
getCategoriesWithOffsetLimit DataTypes.Offset {..} DataTypes.Limit {..} cat = take limit $ drop offset cat
