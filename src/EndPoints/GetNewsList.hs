module EndPoints.GetNewsList
  ( getNewsList,
    newsList,
  )
where

import qualified Control.Exception.Safe as EXS
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Control.Monad.Trans.Except as EX
import Data.Maybe (fromMaybe, isJust)
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
import qualified EndPoints.Lib.ToHttpResponse as ToHttpResponse
import qualified EndPoints.Lib.ToText as ToText
import Logger (logDebug, logInfo, (.<))
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
  Maybe (DataTypes.Id DataTypes.Category) ->
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
  POOL.Pool SQL.Connection ->
  ( News.Handle IO,
    DataTypes.Filter,
    Maybe DataTypes.SortBy,
    Maybe DataTypes.Offset,
    Maybe DataTypes.Limit
  ) ->
  IO (Either ErrorTypes.GetNewsError [DataTypes.News])
newsList pool (h, f, mSort, mo, ml) = do
  let reqResult = EXS.catch (newsListExcept pool (h, f, mSort, mo, ml)) (ErrorTypes.handleGetNewsError h)
  EX.runExceptT reqResult

newsListExcept ::
  POOL.Pool SQL.Connection ->
  ( News.Handle IO,
    DataTypes.Filter,
    Maybe DataTypes.SortBy,
    Maybe DataTypes.Offset,
    Maybe DataTypes.Limit
  ) ->
  EX.ExceptT ErrorTypes.GetNewsError IO [DataTypes.News]
newsListExcept pool (h, f, mSort, mo, ml) = do
  liftIO $ Logger.logInfo (News.hLogHandle h) $ "\n\nGet News List with filter " <> ToText.toText f <> " offset = " .< mo <> " limit = " .< ml
  (offset, limit) <- EX.withExceptT ErrorTypes.InvalidOffsetOrLimitGetNews (EX.catchE (OffsetLimit.checkOffsetLimit h mo ml) (ErrorTypes.handleInvalidOffsetOrLimit h))
  dbFilter <- News.checkFilter f
  dbNews <- newsListFromDb pool offset limit dbFilter
  sortedDbNews <- News.sortNews h mSort dbNews
  news <- Prelude.mapM (NewsIO.toNews pool h) sortedDbNews
  let toTextNews = T.concat $ map ToText.toText news
  liftIO $ Logger.logDebug (News.hLogHandle h) $ "newsListExcept: OK! \n" <> toTextNews
  return news

-- | newsListFromDb  get the full list of news if the array is empty, there is no news
newsListFromDb ::
  POOL.Pool SQL.Connection ->
  DataTypes.Offset ->
  DataTypes.Limit ->
  NewsHelpTypes.DbFilter ->
  EX.ExceptT ErrorTypes.GetNewsError IO [NewsHelpTypes.DbNews]
newsListFromDb pool mo ml f@NewsHelpTypes.DbFilter {..}
  --  category specified in db_filer_category_id
  | isJust dbFilterCategoryId = newsListCategory pool mo ml f
  -- category not specified
  | otherwise = newsListNotCategory pool mo ml f

newsListCategory ::
  POOL.Pool SQL.Connection ->
  DataTypes.Offset ->
  DataTypes.Limit ->
  NewsHelpTypes.DbFilter ->
  EX.ExceptT ErrorTypes.GetNewsError IO [NewsHelpTypes.DbNews]
newsListCategory pool DataTypes.Offset {..} DataTypes.Limit {..} NewsHelpTypes.DbFilter {..} = do
  res <-
    liftIO
      ( EXS.try
          ( POOL.withResource pool $ \conn ->
              SQL.query
                conn
                [sql| SELECT news_title, news_created, usr_name, category_id, category_name, news_text, news_images_id, cardinality (news_images_id), news_published, news_id
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
                ( DataTypes.dayAt dbFilterDayAt,
                  DataTypes.dayUntil dbFilterDayUntil,
                  DataTypes.daySince dbFilterDaySince,
                  dbFilterAuthor,
                  dbFilterTitle,
                  dbFilterContent,
                  newsCat,
                  limit,
                  offset
                )
          ) ::
          IO (Either EXS.SomeException [(T.Text, TIME.Day, T.Text, DataTypes.Id DataTypes.Category, T.Text, T.Text, SQLTypes.PGArray (DataTypes.Id DataTypes.Image), Int, Bool, DataTypes.Id DataTypes.News)])
      )
  case res of
    Left err -> EXS.throwM $ ErrorTypes.GetNewsSomeException err
    Right news -> do
      let dbNews = Prelude.map News.toDbNews news
      return dbNews
  where
    newsCat = fromMaybe (DataTypes.Id {getId = 0}) dbFilterCategoryId

newsListNotCategory ::
  POOL.Pool SQL.Connection ->
  DataTypes.Offset ->
  DataTypes.Limit ->
  NewsHelpTypes.DbFilter ->
  EX.ExceptT ErrorTypes.GetNewsError IO [NewsHelpTypes.DbNews]
newsListNotCategory pool DataTypes.Offset {..} DataTypes.Limit {..} NewsHelpTypes.DbFilter {..} = do
  res <-
    liftIO
      ( EXS.try
          ( POOL.withResource pool $ \conn ->
              SQL.query
                conn
                [sql| SELECT news_title, news_created, usr_name, category_id, category_name, news_text, news_images_id, cardinality (news_images_id), news_published, news_id
            FROM news
            INNER JOIN usr ON news.news_author_login = usr.usr_login INNER JOIN category ON news.news_category_id = category.category_id
            WHERE (news_published = true)
            AND (news_created = ? OR news_created < ? OR news_created > ?)
            AND usr_name LIKE ?
            AND news_title LIKE ?
            AND news_text LIKE ?
            ORDER BY news_created DESC
            LIMIT ?  OFFSET ?|]
                ( DataTypes.dayAt dbFilterDayAt,
                  DataTypes.dayUntil dbFilterDayUntil,
                  DataTypes.daySince dbFilterDaySince,
                  dbFilterAuthor,
                  dbFilterTitle,
                  dbFilterContent,
                  limit,
                  offset
                )
          ) ::
          IO (Either EXS.SomeException [(T.Text, TIME.Day, T.Text, DataTypes.Id DataTypes.Category, T.Text, T.Text, SQLTypes.PGArray (DataTypes.Id DataTypes.Image), Int, Bool, DataTypes.Id DataTypes.News)])
      )
  case res of
    Left err -> EXS.throwM $ ErrorTypes.GetNewsSomeException err
    Right news -> do
      let dbNews = Prelude.map News.toDbNews news
      return dbNews
