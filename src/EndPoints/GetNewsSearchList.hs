module EndPoints.GetNewsSearchList
  ( getNewsSearchList,
    newsSearchList,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Control.Monad.Trans.Except as EX
import qualified Data.Pool as POOL
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as SQL
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified DbConnect
import qualified EndPoints.Lib.News.News as News
import qualified EndPoints.Lib.News.NewsHelpTypes as NewsHelpTypes
import qualified EndPoints.Lib.News.NewsIO as NewsIO
import qualified EndPoints.Lib.OffsetLimit as OffsetLimit
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
  DataTypes.StatePool ->
  ( News.Handle IO,
    Maybe T.Text,
    Maybe DataTypes.Offset,
    Maybe DataTypes.Limit
  ) ->
  IO (Either ErrorTypes.GetNewsError [DataTypes.News])
newsSearchList conn (h, search, mo, ml) = undefined

{--
newsSearchList ::
  SQL.Connection ->
  ( News.Handle IO,
    Maybe T.Text,
    Maybe DataTypes.Offset,
    Maybe DataTypes.Limit
  ) ->
  IO (Either ErrorTypes.GetNewsError [DataTypes.News])
newsSearchList conn (h, search, mo, ml) = do EX.runExceptT $ newsSearchListExcept conn (h, search, mo, ml)

newsSearchListExcept ::
  SQL.Connection ->
  ( News.Handle IO,
    Maybe T.Text,
    Maybe DataTypes.Offset,
    Maybe DataTypes.Limit
  ) ->
  EX.ExceptT ErrorTypes.GetNewsError IO [DataTypes.News]
newsSearchListExcept _ (h, Nothing, _, _) = do
  liftIO $ Logger.logError (News.hLogHandle h) ("ERROR " .< ErrorTypes.InvalidSearchGetNews (ErrorTypes.InvalidRequest "newsSearchListExcept: BAD! Not text for searching \n"))
  EX.throwE $ ErrorTypes.InvalidSearchGetNews $ ErrorTypes.InvalidRequest []
newsSearchListExcept _ (h, Just search, mo, ml) = do
  liftIO $ Logger.logInfo (News.hLogHandle h) $ T.concat ["Request: Get News Search List ", search, " offset = ", T.pack $ show mo, " limit = ", T.pack $ show ml]
  (offset, limit) <- EX.withExceptT ErrorTypes.InvalidOffsetOrLimitGetNews $ OffsetLimit.checkOffsetLimit h mo ml
  conn <- EX.withExceptT ErrorTypes.GetNewsSQLRequestError $ DbConnect.tryRequestConnectDb h
  res <- newsSearchListAtDb conn h search offset limit
  news <- Prelude.mapM (NewsIO.toNews conn h) res
  let toTextNews = T.concat $ map ToText.toText news
  liftIO $ Logger.logInfo (News.hLogHandle h) $ T.concat ["newsSearchListExcept: OK! \n", toTextNews]
  return news

newsSearchListAtDb ::
  SQL.Connection ->
  News.Handle IO ->
  T.Text ->
  DataTypes.Offset ->
  DataTypes.Limit ->
  EX.ExceptT ErrorTypes.GetNewsError IO [NewsHelpTypes.DbNews]
newsSearchListAtDb conn _ search off lim = do
  res <-
    liftIO $
      SQL.query
        conn
        [sql| SELECT news_title, news_created, usr_name, category_path, category_name, news_text, news_images_id, cardinality (news_images_id), news_published, news_id
            FROM news
            INNER JOIN usr ON news.news_author_login = usr.usr_login INNER JOIN category ON news.news_category_id = category.category_id
            where (news_published = true) and (to_tsvector(news_title) || to_tsvector(usr_name) || to_tsvector(category_name) || to_tsvector(news_text) @@ plainto_tsquery(?))
            ORDER BY news_created DESC
            LIMIT ?  OFFSET ? |]
        (search, show lim, show off)
  let dbNews = Prelude.map News.toDbNews res
  return dbNews
--}