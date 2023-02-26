module EndPoints.GetAuthorsNewsSearchList
  ( getAuthorsNewsSearchList,
    authorsNewsSearchList,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Control.Monad.Trans.Except as EX
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as SQL
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified EndPoints.Lib.Lib as Lib
import qualified EndPoints.Lib.News.News as News
import qualified EndPoints.Lib.News.NewsHelpTypes as NewsHelpTypes
import qualified EndPoints.Lib.News.NewsIO as NewsIO
import qualified EndPoints.Lib.OffsetLimit as OffsetLimit
import qualified EndPoints.Lib.ToHttpResponse as ToHttpResponse
import qualified EndPoints.Lib.ToText as ToText
import Logger (logError, (.<))
import qualified Logger
import qualified News
import Servant (Handler)
import qualified Types.DataTypes as DataTypes
import qualified Types.ErrorTypes as ErrorTypes

getAuthorsNewsSearchList ::
  News.Handle IO ->
  DataTypes.Db ->
  DataTypes.User ->
  Maybe T.Text ->
  Maybe DataTypes.Offset ->
  Maybe DataTypes.Limit ->
  Handler [DataTypes.News]
getAuthorsNewsSearchList h DataTypes.Db {..} user search' mo ml =
  (>>=)
    (liftIO $ dbAuthorsNewsSearchList (h, user, search', mo, ml))
    ToHttpResponse.toHttpResponse

authorsNewsSearchList ::
  SQL.Connection ->
  ( News.Handle IO,
    DataTypes.User,
    Maybe T.Text,
    Maybe DataTypes.Offset,
    Maybe DataTypes.Limit
  ) ->
  IO (Either ErrorTypes.GetNewsError [DataTypes.News])
authorsNewsSearchList conn (h, user, search, mo, ml) = do EX.runExceptT $ authorsNewsSearchListExcept conn (h, user, search, mo, ml)

authorsNewsSearchListExcept ::
  SQL.Connection ->
  ( News.Handle IO,
    DataTypes.User,
    Maybe T.Text,
    Maybe DataTypes.Offset,
    Maybe DataTypes.Limit
  ) ->
  EX.ExceptT ErrorTypes.GetNewsError IO [DataTypes.News]
authorsNewsSearchListExcept _ (h, _, Nothing, _, _) = do
  liftIO $ Logger.logError (News.hLogHandle h) ("ERROR " .< ErrorTypes.InvalidSearchGetNews (ErrorTypes.InvalidRequest "authorsNewsSearchListExcept: BAD! Not text for searching \n"))
  EX.throwE $ ErrorTypes.InvalidSearchGetNews $ ErrorTypes.InvalidRequest []
authorsNewsSearchListExcept conn (h, user, Just search, mo, ml) = do
  liftIO $ Logger.logInfo (News.hLogHandle h) $ T.concat ["Request with authentication: Get News Search List ", search, " offset = ", T.pack $ show mo, " limit = ", T.pack $ show ml]
  (offset, limit) <- allCheck (h, user, mo, ml)
  dbNews <- authorsNewsSearchListFromDb conn h user search offset limit
  news <- Prelude.mapM (NewsIO.toNews conn h) dbNews
  let toTextNews = T.concat $ map ToText.toText news
  liftIO $ Logger.logDebug (News.hLogHandle h) $ T.concat ["authorsNewsSearchListExcept: OK! \n", toTextNews]
  return news

allCheck ::
  Monad m =>
  ( News.Handle m,
    DataTypes.User,
    Maybe DataTypes.Offset,
    Maybe DataTypes.Limit
  ) ->
  EX.ExceptT ErrorTypes.GetNewsError m (DataTypes.Offset, DataTypes.Limit)
allCheck (h, user, mo, ml) = do
  let checkUserAuthor = EX.withExceptT ErrorTypes.InvalidPermissionGetNews (Lib.checkUserAuthor h user)
      checkOffsetLimit = EX.withExceptT ErrorTypes.InvalidOffsetOrLimitGetNews $ OffsetLimit.checkOffsetLimit h mo ml
  checkUserAuthor >> checkOffsetLimit

authorsNewsSearchListFromDb ::
  SQL.Connection ->
  News.Handle IO ->
  DataTypes.User ->
  T.Text ->
  DataTypes.Offset ->
  DataTypes.Limit ->
  EX.ExceptT ErrorTypes.GetNewsError IO [NewsHelpTypes.DbNews]
authorsNewsSearchListFromDb conn _ DataTypes.User {..} search off lim = do
  res <-
    liftIO $
      SQL.query
        conn
        [sql| SELECT news_title, news_created, usr_name, category_path, category_name, news_text, news_images_id, cardinality (news_images_id), news_published , news_id
            FROM news
            INNER JOIN usr ON news.news_author_login = usr.usr_login INNER JOIN category ON news.news_category_id = category.category_id
            where ((news_published = true) or (news_author_login = ? )) and (to_tsvector(news_title) || to_tsvector(usr_name) || to_tsvector(category_name) || to_tsvector(news_text) @@ plainto_tsquery(?))
            ORDER BY news_created DESC 
            LIMIT ?  OFFSET ? |]
        (userLogin, search, show lim, show off)
  let dbNews = Prelude.map News.toDbNews res
  return dbNews
