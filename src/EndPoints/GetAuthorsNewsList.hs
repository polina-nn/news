module EndPoints.GetAuthorsNewsList
  ( getAuthorsNewsList,
    authorsNewsList,
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
import qualified EndPoints.Lib.LibIO as LibIO
import qualified EndPoints.Lib.News.News as News
import qualified EndPoints.Lib.News.NewsHelpTypes as NewsHelpTypes
import qualified EndPoints.Lib.News.NewsIO as NewsIO
import qualified EndPoints.Lib.ThrowSqlRequestError as Throw
import qualified EndPoints.Lib.ToHttpResponse as ToHttpResponse
import qualified EndPoints.Lib.ToText as ToText
import Logger (logDebug, logInfo)
import qualified News
import Servant (Handler)
import qualified Types.DataTypes as DataTypes
import qualified Types.ErrorTypes as ErrorTypes

getAuthorsNewsList ::
  News.Handle IO ->
  DataTypes.Db ->
  DataTypes.Token ->
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
getAuthorsNewsList h DataTypes.Db {..} user da du ds ar i t c mSort mo ml =
  (>>=)
    (liftIO $ dbAuthorsNewsList (h, user, filter', mSort, mo, ml))
    ToHttpResponse.toHttpResponse
  where
    filter' :: DataTypes.Filter
    filter' = News.toFilter da du ds ar i t c

authorsNewsList ::
  POOL.Pool SQL.Connection ->
  ( News.Handle IO,
    DataTypes.Token,
    DataTypes.Filter,
    Maybe DataTypes.SortBy,
    Maybe DataTypes.Offset,
    Maybe DataTypes.Limit
  ) ->
  IO (Either ErrorTypes.GetNewsError [DataTypes.News])
authorsNewsList pool (h, token, f, mSort, mo, ml) = do EX.runExceptT $ authorsNewsListExcept pool (h, token, f, mSort, mo, ml)

authorsNewsListExcept ::
  POOL.Pool SQL.Connection ->
  ( News.Handle IO,
    DataTypes.Token,
    DataTypes.Filter,
    Maybe DataTypes.SortBy,
    Maybe DataTypes.Offset,
    Maybe DataTypes.Limit
  ) ->
  EX.ExceptT ErrorTypes.GetNewsError IO [DataTypes.News]
authorsNewsListExcept pool (h, token, f, mSort, mo, ml) = do
  user <- EX.withExceptT ErrorTypes.GetNewsSQLRequestError (LibIO.searchUser h pool token)
  liftIO $ Logger.logInfo (News.hLogHandle h) $ T.concat ["Request with authentication: Get News List with filter ", ToText.toText f, " offset = ", T.pack $ show mo, " limit = ", T.pack $ show ml]
  (offset, limit, dbFiler) <- News.checkUserOffsetLimitFilter (h, user, f, mo, ml)
  dbNews <- authorsNewsListFromDb pool h user offset limit dbFiler
  sortedDbNews <- News.sortNews h mSort dbNews
  news <- Prelude.mapM (NewsIO.toNews pool h) sortedDbNews
  let toTextNews = T.concat $ map ToText.toText news
  liftIO $ Logger.logDebug (News.hLogHandle h) $ T.concat ["authorsNewsSearchListExcept: OK! \n", toTextNews]
  return news

-- | authorsNewsListFromDb  get the full list of news if the array is empty, there is no news
authorsNewsListFromDb ::
  POOL.Pool SQL.Connection ->
  News.Handle IO ->
  DataTypes.User ->
  DataTypes.Offset ->
  DataTypes.Limit ->
  NewsHelpTypes.DbFilter ->
  EX.ExceptT ErrorTypes.GetNewsError IO [NewsHelpTypes.DbNews]
authorsNewsListFromDb pool h user mo ml f@NewsHelpTypes.DbFilter {..}
  --  category specified in db_filer_category_id
  | isJust dbFilterCategoryId = authorsNewsListCategory pool h user mo ml f
  -- category not specified
  | otherwise = authorsNewsListNotCategory pool h user mo ml f

authorsNewsListCategory ::
  POOL.Pool SQL.Connection ->
  News.Handle IO ->
  DataTypes.User ->
  DataTypes.Offset ->
  DataTypes.Limit ->
  NewsHelpTypes.DbFilter ->
  EX.ExceptT ErrorTypes.GetNewsError IO [NewsHelpTypes.DbNews]
authorsNewsListCategory pool h DataTypes.User {..} off lim NewsHelpTypes.DbFilter {..} = do
  res <-
    liftIO
      ( EXS.try
          ( POOL.withResource pool $ \conn ->
              SQL.query
                conn
                [sql| SELECT news_title, news_created, usr_name, category_path, category_name, news_text, news_images_id, cardinality (news_images_id), news_published, news_id
            FROM news
            INNER JOIN usr ON news.news_author_login = usr.usr_login INNER JOIN category ON news.news_category_id = category.category_id
            WHERE ((news_published = true) OR (news_author_login = ? )) 
            AND (news_created = ? OR news_created < ? OR news_created > ?) 
            AND usr_name LIKE ?
            AND news_title LIKE ?
            AND news_text LIKE ?
            AND news_category_id = ?
            ORDER BY news_created DESC 
            LIMIT ?  OFFSET ?|]
                ( userLogin,
                  dbFilterDayAt,
                  dbFilterDayUntil,
                  dbFilterDaySince,
                  dbFilterAuthor,
                  dbFilterTitle,
                  dbFilterContent,
                  newsCat,
                  lim,
                  off
                )
          ) ::
          IO (Either EXS.SomeException [(T.Text, TIME.Day, T.Text, String, T.Text, T.Text, SQLTypes.PGArray Int, Int, Bool, Int)])
      )
  case res of
    Left err -> Throw.throwSqlRequestError h ("authorsNewsListCategory", show err)
    Right news -> do
      let dbNews = Prelude.map News.toDbNews news
      return dbNews
  where
    newsCat = fromMaybe 0 dbFilterCategoryId

authorsNewsListNotCategory ::
  POOL.Pool SQL.Connection ->
  News.Handle IO ->
  DataTypes.User ->
  DataTypes.Offset ->
  DataTypes.Limit ->
  NewsHelpTypes.DbFilter ->
  EX.ExceptT ErrorTypes.GetNewsError IO [NewsHelpTypes.DbNews]
authorsNewsListNotCategory pool h DataTypes.User {..} off lim NewsHelpTypes.DbFilter {..} = do
  res <-
    liftIO
      ( EXS.try
          ( POOL.withResource pool $ \conn ->
              SQL.query
                conn
                [sql| SELECT news_title, news_created, usr_name, category_path, category_name, news_text, news_images_id, cardinality (news_images_id), news_published , news_id
            FROM news
            INNER JOIN usr ON news.news_author_login = usr.usr_login INNER JOIN category ON news.news_category_id = category.category_id
            WHERE ((news_published = true) OR (news_author_login = ? )) 
            AND (news_created = ? OR news_created < ? OR news_created > ?) 
            AND usr_name LIKE ?
            AND news_title LIKE ?
            AND news_text LIKE ?
            ORDER BY news_created DESC 
            LIMIT ?  OFFSET ?|]
                ( userLogin,
                  dbFilterDayAt,
                  dbFilterDayUntil,
                  dbFilterDaySince,
                  dbFilterAuthor,
                  dbFilterTitle,
                  dbFilterContent,
                  lim,
                  off
                )
          ) ::
          IO (Either EXS.SomeException [(T.Text, TIME.Day, T.Text, String, T.Text, T.Text, SQLTypes.PGArray Int, Int, Bool, Int)])
      )
  case res of
    Left err -> Throw.throwSqlRequestError h ("authorsNewsListNotCategory", show err)
    Right news -> do
      let dbNews = Prelude.map News.toDbNews news
      return dbNews
