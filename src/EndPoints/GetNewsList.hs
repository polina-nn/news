module EndPoints.GetNewsList
  ( getNewsList,
    newsList,
  )
where

import qualified Control.Exception.Safe as EXS
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Control.Monad.Trans.Except as EX
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as T
import qualified Data.Time as TIME
import qualified Database.PostgreSQL.Simple as SQL
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified Database.PostgreSQL.Simple.Types as SQLTypes
import qualified EndPoints.Lib.News.News as News
import qualified EndPoints.Lib.News.NewsHelpTypes as NewsHelpTypes
import qualified EndPoints.Lib.News.NewsIO as NewsIO
import qualified EndPoints.Lib.ToHttpResponse as ToHttpResponse
import qualified EndPoints.Lib.ToText as ToText
import Logger (logDebug, logError, logInfo, (.<))
import qualified News
import Servant (Handler)
import qualified Types.DataTypes as DataTypes
import qualified Types.ErrorTypes as ErrorTypes

getNewsList ::
  News.Handle IO ->
  DataTypes.Db ->
  Maybe DataTypes.DayAt ->
  Maybe DataTypes.DayUntil ->
  Maybe DataTypes.DaySince ->
  Maybe T.Text ->
  Maybe Int ->
  Maybe T.Text ->
  Maybe T.Text ->
  Maybe DataTypes.SortBy ->
  Maybe DataTypes.Offset ->
  Maybe DataTypes.Limit ->
  Handler [DataTypes.News]
getNewsList h DataTypes.Db {..} da du ds ar i t c mSort mo ml =
  (>>=)
    (liftIO $ dbNewsList (h, filter', mSort, mo, ml))
    ToHttpResponse.toHttpResponse
  where
    filter' :: DataTypes.Filter
    filter' = News.toFilter da du ds ar i t c

newsList ::
  SQL.Connection ->
  ( News.Handle IO,
    DataTypes.Filter,
    Maybe DataTypes.SortBy,
    Maybe DataTypes.Offset,
    Maybe DataTypes.Limit
  ) ->
  IO (Either ErrorTypes.GetNewsError [DataTypes.News])
newsList conn (h, f, mSort, mo, ml) = do EX.runExceptT $ newsListExcept conn (h, f, mSort, mo, ml)

newsListExcept ::
  SQL.Connection ->
  ( News.Handle IO,
    DataTypes.Filter,
    Maybe DataTypes.SortBy,
    Maybe DataTypes.Offset,
    Maybe DataTypes.Limit
  ) ->
  EX.ExceptT ErrorTypes.GetNewsError IO [DataTypes.News]
newsListExcept conn (h, f, mSort, mo, ml) = do
  liftIO $ Logger.logInfo (News.hLogHandle h) $ T.concat ["Get News List with filter ", ToText.toText f, " offset = ", T.pack $ show mo, " limit = ", T.pack $ show ml]
  (offset, limit, dbFilter) <- News.checkOffsetLimitFilter (h, f, mo, ml)
  liftIO $ Logger.logDebug (News.hLogHandle h) $ T.concat ["dbFilter: OK! \n", T.pack $ show dbFilter]
  dbNews <- newsListFromDb conn h offset limit dbFilter
  sortedDbNews <- News.sortNews h mSort dbNews
  news <- Prelude.mapM (NewsIO.toNews conn h) sortedDbNews
  let toTextNews = T.concat $ map ToText.toText news
  liftIO $ Logger.logDebug (News.hLogHandle h) $ T.concat ["newsListExcept: OK! \n", toTextNews]
  return news

-- | newsListFromDb  get the full list of news if the array is empty, there is no news
newsListFromDb ::
  SQL.Connection ->
  News.Handle IO ->
  DataTypes.Offset ->
  DataTypes.Limit ->
  NewsHelpTypes.DbFilter ->
  EX.ExceptT ErrorTypes.GetNewsError IO [NewsHelpTypes.DbNews]
newsListFromDb conn h mo ml f@NewsHelpTypes.DbFilter {..}
  --  category specified in db_filer_category_id
  | isJust dbFilterCategoryId = newsListCategory conn h mo ml f
  -- category not specified
  | otherwise = newsListNotCategory conn h mo ml f

newsListCategory ::
  SQL.Connection ->
  News.Handle IO ->
  DataTypes.Offset ->
  DataTypes.Limit ->
  NewsHelpTypes.DbFilter ->
  EX.ExceptT ErrorTypes.GetNewsError IO [NewsHelpTypes.DbNews]
newsListCategory conn h off lim NewsHelpTypes.DbFilter {..} = do
  res <-
    liftIO
      ( EXS.try $
          SQL.query
            conn
            [sql| SELECT news_title, news_created, usr_name, category_path, category_name, news_text, news_images_id, cardinality (news_images_id), news_published, news_id
            FROM news
            INNER JOIN usr ON news.news_author_login = usr.usr_login INNER JOIN category ON news.news_category_id = category.category_id
            WHERE (news_published = true) 
            AND (news_created = ? OR news_created < ? OR news_created > ?) 
            AND usr_name LIKE ?
            AND news_title LIKE ?
            AND news_text LIKE ?
            AND news_category_id = ?
            ORDER BY news_created DESC 
            LIMIT ?  OFFSET ?|]
            ( dbFilterDayAt,
              dbFilterDayUntil,
              dbFilterDaySince,
              dbFilterAuthor,
              dbFilterTitle,
              dbFilterContent,
              newsCat,
              lim,
              off
            ) ::
          IO (Either SQL.SqlError [(T.Text, TIME.Day, T.Text, String, T.Text, T.Text, SQLTypes.PGArray Int, Int, Bool, Int)])
      )
  case res of
    Left _ -> do
      liftIO $ Logger.logError (News.hLogHandle h) ("ERROR " .< ErrorTypes.SQLRequestError "newsListCategory: BAD! Connection disconnected. Restore the connection to the database and run the server again! ")
      EX.throwE $ ErrorTypes.GetNewsSQLRequestError $ ErrorTypes.SQLRequestError []
    Right news -> do
      let dbNews = Prelude.map News.toDbNews news
      return dbNews
  where
    newsCat = fromMaybe 0 dbFilterCategoryId

newsListNotCategory ::
  SQL.Connection ->
  News.Handle IO ->
  DataTypes.Offset ->
  DataTypes.Limit ->
  NewsHelpTypes.DbFilter ->
  EX.ExceptT ErrorTypes.GetNewsError IO [NewsHelpTypes.DbNews]
newsListNotCategory conn h off lim NewsHelpTypes.DbFilter {..} = do
  res <-
    liftIO
      ( EXS.try $
          SQL.query
            conn
            [sql| SELECT news_title, news_created, usr_name, category_path, category_name, news_text, news_images_id, cardinality (news_images_id), news_published, news_id
            FROM news
            INNER JOIN usr ON news.news_author_login = usr.usr_login INNER JOIN category ON news.news_category_id = category.category_id
            WHERE (news_published = true) 
            AND (news_created = ? OR news_created < ? OR news_created > ?) 
            AND usr_name LIKE ?
            AND news_title LIKE ?
            AND news_text LIKE ?
            ORDER BY news_created DESC 
            LIMIT ?  OFFSET ?|]
            ( dbFilterDayAt,
              dbFilterDayUntil,
              dbFilterDaySince,
              dbFilterAuthor,
              dbFilterTitle,
              dbFilterContent,
              lim,
              off
            ) ::
          IO (Either SQL.SqlError [(T.Text, TIME.Day, T.Text, String, T.Text, T.Text, SQLTypes.PGArray Int, Int, Bool, Int)])
      )
  case res of
    Left err -> do
      liftIO $ Logger.logError (News.hLogHandle h) ("ERROR " .< ErrorTypes.SQLRequestError ("newsListCategory: BAD!" <> show err))
      EX.throwE $ ErrorTypes.GetNewsSQLRequestError $ ErrorTypes.SQLRequestError []
    Right news -> do
      let dbNews = Prelude.map News.toDbNews news
      return dbNews
