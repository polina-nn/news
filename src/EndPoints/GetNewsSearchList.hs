module EndPoints.GetNewsSearchList
  ( getNewsSearchList,
    newsSearchList,
  )
where

import qualified Control.Exception.Safe as EXS
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Control.Monad.Trans.Except as EX
import qualified Data.Pool as POOL
import qualified Data.Text as T
import qualified Data.Time as TIME
import qualified Database.PostgreSQL.Simple as SQL
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified Database.PostgreSQL.Simple.Types as SQLTypes
import qualified EndPoints.Lib.News.News as News
import qualified EndPoints.Lib.News.NewsHelpTypes as NewsHelpTypes
import qualified EndPoints.Lib.News.NewsIO as NewsIO
import qualified EndPoints.Lib.OffsetLimit as OffsetLimit
import qualified EndPoints.Lib.ThrowSqlRequestError as Throw
import qualified EndPoints.Lib.ToHttpResponse as ToHttpResponse
import qualified EndPoints.Lib.ToText as ToText
import Logger (logError, logInfo, (.<))
import qualified News
import Servant (Handler)
import qualified Types.DataTypes as DataTypes
import qualified Types.ErrorTypes as ErrorTypes

getNewsSearchList ::
  News.Handle IO ->
  DataTypes.Db ->
  Maybe T.Text ->
  Maybe DataTypes.Offset ->
  Maybe DataTypes.Limit ->
  Handler [DataTypes.News]
getNewsSearchList h DataTypes.Db {..} mSearch mo ml =
  (>>=)
    (liftIO $ dbNewsSearchList (h, mSearch, mo, ml))
    ToHttpResponse.toHttpResponse

newsSearchList ::
  POOL.Pool SQL.Connection ->
  ( News.Handle IO,
    Maybe T.Text,
    Maybe DataTypes.Offset,
    Maybe DataTypes.Limit
  ) ->
  IO (Either ErrorTypes.GetNewsError [DataTypes.News])
newsSearchList pool (h, search, mo, ml) = do EX.runExceptT $ newsSearchListExcept pool (h, search, mo, ml)

newsSearchListExcept ::
  POOL.Pool SQL.Connection ->
  ( News.Handle IO,
    Maybe T.Text,
    Maybe DataTypes.Offset,
    Maybe DataTypes.Limit
  ) ->
  EX.ExceptT ErrorTypes.GetNewsError IO [DataTypes.News]
newsSearchListExcept _ (h, Nothing, _, _) = do
  liftIO $ Logger.logError (News.hLogHandle h) ("ERROR " .< ErrorTypes.InvalidSearchGetNews (ErrorTypes.InvalidRequest "newsSearchListExcept: BAD! Not text for searching \n"))
  EX.throwE $ ErrorTypes.InvalidSearchGetNews $ ErrorTypes.InvalidRequest []
newsSearchListExcept pool (h, Just search, mo, ml) = do
  liftIO $ Logger.logInfo (News.hLogHandle h) $ T.concat ["Request: Get News Search List ", search, " offset = ", T.pack $ show mo, " limit = ", T.pack $ show ml]
  (offset, limit) <- EX.withExceptT ErrorTypes.InvalidOffsetOrLimitGetNews $ OffsetLimit.checkOffsetLimit h mo ml
  res <- newsSearchListAtDb pool h search offset limit
  news <- Prelude.mapM (NewsIO.toNews pool h) res
  let toTextNews = T.concat $ map ToText.toText news
  liftIO $ Logger.logInfo (News.hLogHandle h) $ T.concat ["newsSearchListExcept: OK! \n", toTextNews]
  return news

newsSearchListAtDb ::
  POOL.Pool SQL.Connection ->
  News.Handle IO ->
  T.Text ->
  DataTypes.Offset ->
  DataTypes.Limit ->
  EX.ExceptT ErrorTypes.GetNewsError IO [NewsHelpTypes.DbNews]
newsSearchListAtDb pool h search off lim = do
  res <-
    liftIO
      ( POOL.withResource pool $ \conn ->
          EXS.try $
            SQL.query
              conn
              [sql| SELECT news_title, news_created, usr_name, category_path, category_name, news_text, news_images_id, cardinality (news_images_id), news_published, news_id
            FROM news
            INNER JOIN usr ON news.news_author_login = usr.usr_login INNER JOIN category ON news.news_category_id = category.category_id
            where (news_published = true) and (to_tsvector(news_title) || to_tsvector(usr_name) || to_tsvector(category_name) || to_tsvector(news_text) @@ plainto_tsquery(?))
            ORDER BY news_created DESC 
            LIMIT ?  OFFSET ? |]
              (search, show lim, show off) ::
            IO (Either EXS.SomeException [(T.Text, TIME.Day, T.Text, String, T.Text, T.Text, SQLTypes.PGArray Int, Int, Bool, Int)])
      )
  case res of
    Left err -> Throw.throwSqlRequestError h ("newsSearchListAtDb", show err)
    Right newsList -> do
      let dbNews = Prelude.map News.toDbNews newsList
      return dbNews
