module EndPoints.GetAuthorsNewsList
  ( getAuthorsNewsList,
    authorsNewsList,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as SQL
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified EndPoints.Lib.News.News as News
import qualified EndPoints.Lib.News.NewsHelpTypes as NewsHelpTypes
import qualified EndPoints.Lib.News.NewsIO as NewsIO
import qualified EndPoints.Lib.ToHttpResponse as ToHttpResponse
import qualified EndPoints.Lib.ToText as ToText
import Logger (logDebug, logError, logInfo)
import qualified News
import Servant (Handler)
import qualified Types.DataTypes as DataTypes
import qualified Types.ErrorTypes as ErrorTypes

getAuthorsNewsList ::
  News.Handle IO ->
  DataTypes.Db ->
  DataTypes.User ->
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
    (liftIO $ _authorsNewsList (h, user, filter', mSort, mo, ml))
    ToHttpResponse.toHttpResponse
  where
    filter' :: DataTypes.Filter
    filter' = News.toFilter da du ds ar i t c

authorsNewsList ::
  SQL.Connection ->
  ( News.Handle IO,
    DataTypes.User,
    DataTypes.Filter,
    Maybe DataTypes.SortBy,
    Maybe DataTypes.Offset,
    Maybe DataTypes.Limit
  ) ->
  IO (Either ErrorTypes.GetNewsError [DataTypes.News])
authorsNewsList conn (h, user, f, mSort, mo, ml) = do
  Logger.logInfo (News.hLogHandle h) $ T.concat ["Request with authentication: Get News List with filter ", ToText.toText f, " offset = ", T.pack $ show mo, " limit = ", T.pack $ show ml]
  resAllCheck <- News.checkUserOffsetLimitFilter (h, user, f, mo, ml)
  case resAllCheck of
    Left err -> do
      Logger.logError (News.hLogHandle h) "All Check: BAD!  \n"
      return $ Left err
    Right (offset, limit, dbFiler) -> do
      Logger.logDebug (News.hLogHandle h) "All Check: OK!  \n"
      res <-
        authorsNewsList' conn user offset limit dbFiler
          >>= News.sortNews h mSort
      news <- Prelude.mapM (NewsIO.toNews conn h) res
      case News.checkErrorsToNews news res of
        (True, news') -> do
          let toTextNews = T.concat $ map ToText.toText news'
          Logger.logInfo (News.hLogHandle h) $ T.concat ["newsList: OK! \n", toTextNews]
          return $ Right news'
        _ ->
          return $
            Left $
              ErrorTypes.GetNewsSQLRequestError $ ErrorTypes.SQLRequestError []

-- | authorsNewsList'  get the full list of news if the array is empty, there is no news
authorsNewsList' ::
  SQL.Connection ->
  DataTypes.User ->
  DataTypes.Offset ->
  DataTypes.Limit ->
  NewsHelpTypes.DbFilter ->
  IO [NewsHelpTypes.DbNews]
authorsNewsList' conn user mo ml f@NewsHelpTypes.DbFilter {..}
  --  category specified in db_filer_category_id
  | isJust dbFilterCategoryId = authorsNewsListCategory conn user mo ml f
  -- category not specified
  | otherwise = authorsNewsListNotCategory conn user mo ml f

authorsNewsListCategory ::
  SQL.Connection ->
  DataTypes.User ->
  DataTypes.Offset ->
  DataTypes.Limit ->
  NewsHelpTypes.DbFilter ->
  IO [NewsHelpTypes.DbNews]
authorsNewsListCategory conn DataTypes.User {..} off lim NewsHelpTypes.DbFilter {..} = do
  res <-
    SQL.query
      conn
      [sql| SELECT news_title, news_created, usr_name, category_path, category_name, news_text, news_images_id, cardinality (news_images_id), news_published
            FROM news
            INNER JOIN usr ON news.news_author_login = usr.usr_login INNER JOIN category ON news.news_category_id = category.category_id
            WHERE ((news_published = true) OR (news_author_login = ? )) 
            AND (news_created = ? OR news_created < ? OR news_created > ?) 
            AND usr_name LIKE ?
            AND news_title LIKE ?
            AND news_text LIKE ?
            AND news_category_id = ?
            ORDER BY news_created DESC 
            LIMIT ?  OFFSET ?;|]
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
  let dbNews = Prelude.map News.toDbNews res
  return dbNews
  where
    newsCat = fromMaybe 0 dbFilterCategoryId

authorsNewsListNotCategory ::
  SQL.Connection ->
  DataTypes.User ->
  DataTypes.Offset ->
  DataTypes.Limit ->
  NewsHelpTypes.DbFilter ->
  IO [NewsHelpTypes.DbNews]
authorsNewsListNotCategory conn DataTypes.User {..} off lim NewsHelpTypes.DbFilter {..} = do
  res <-
    SQL.query
      conn
      [sql| SELECT news_title, news_created, usr_name, category_path, category_name, news_text, news_images_id, cardinality (news_images_id), news_published
            FROM news
            INNER JOIN usr ON news.news_author_login = usr.usr_login INNER JOIN category ON news.news_category_id = category.category_id
            WHERE ((news_published = true) OR (news_author_login = ? )) 
            AND (news_created = ? OR news_created < ? OR news_created > ?) 
            AND usr_name LIKE ?
            AND news_title LIKE ?
            AND news_text LIKE ?
            ORDER BY news_created DESC 
            LIMIT ?  OFFSET ?;|]
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
  let dbNews = Prelude.map News.toDbNews res
  return dbNews
