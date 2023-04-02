module EndPoints.GetCategoryList
  ( getCategoryList,
    categoryList,
  )
where

import qualified Control.Exception.Safe as EXS
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Control.Monad.Trans.Except as EX
import qualified Data.Pool as POOL
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as SQL
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified EndPoints.Lib.Category.Category as Category
import qualified EndPoints.Lib.OffsetLimit as OffsetLimit
import qualified EndPoints.Lib.ThrowSqlRequestError as Throw
import qualified EndPoints.Lib.ToHttpResponse as ToHttpResponse
import qualified EndPoints.Lib.ToText as ToText
import Logger (logDebug, logInfo)
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
  liftIO $ Logger.logInfo (News.hLogHandle h) $ T.concat ["Request: Get Category List  with offset = ", T.pack $ show mo, " limit = ", T.pack $ show ml]
  (offset, limit) <- EX.withExceptT ErrorTypes.InvalidOffsetOrLimitGetContent $ OffsetLimit.checkOffsetLimit h mo ml
  res <-
    liftIO
      ( POOL.withResource pool $ \conn ->
          EXS.try $
            SQL.query
              conn
              [sql| SELECT category_path, category_id, category_name
                FROM category 
                ORDER BY category_path 
                LIMIT ?  OFFSET ? |]
              (show limit, show offset) ::
            IO (Either EXS.SomeException [(DataTypes.Path, DataTypes.Id, DataTypes.Name)])
      )
  case res of
    Left err -> Throw.throwSqlRequestError h ("checkId", show err)
    Right value -> do
      let categories = Prelude.map Category.toCategories value
          toTextCategories = T.concat $ map ToText.toText categories
      liftIO $ Logger.logDebug (News.hLogHandle h) $ T.concat ["categoryListExcept: OK! \n", toTextCategories]
      return categories
