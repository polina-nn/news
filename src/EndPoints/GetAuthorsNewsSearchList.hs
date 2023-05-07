module EndPoints.GetAuthorsNewsSearchList
  ( getAuthorsNewsSearchList,
    authorsNewsSearchList,
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
import qualified EndPoints.Lib.Lib as Lib
import qualified EndPoints.Lib.LibIO as LibIO
import qualified EndPoints.Lib.News.News as News
import qualified EndPoints.Lib.News.NewsHelpTypes as NewsHelpTypes
import qualified EndPoints.Lib.News.NewsIO as NewsIO
import qualified EndPoints.Lib.OffsetLimit as OffsetLimit
import qualified EndPoints.Lib.ThrowError as Throw
import qualified EndPoints.Lib.ToHttpResponse as ToHttpResponse
import qualified EndPoints.Lib.ToText as ToText
import Logger ((.<))
import qualified Logger
import qualified News
import Servant (Handler)
import qualified Types.DataTypes as DataTypes
import qualified Types.ErrorTypes as ErrorTypes

getAuthorsNewsSearchList ::
  News.Handle IO ->
  DataTypes.Db ->
  DataTypes.Token ->
  Maybe T.Text ->
  Maybe DataTypes.Offset ->
  Maybe DataTypes.Limit ->
  Handler [DataTypes.News]
getAuthorsNewsSearchList h DataTypes.Db {..} user search' mo ml =
  (>>=)
    (liftIO $ dbAuthorsNewsSearchList (h, user, search', mo, ml))
    ToHttpResponse.toHttpResponse

authorsNewsSearchList ::
  POOL.Pool SQL.Connection ->
  ( News.Handle IO,
    DataTypes.Token,
    Maybe T.Text,
    Maybe DataTypes.Offset,
    Maybe DataTypes.Limit
  ) ->
  IO (Either ErrorTypes.GetNewsError [DataTypes.News])
authorsNewsSearchList pool (h, token, search, mo, ml) = EX.runExceptT $ authorsNewsSearchListExcept pool (h, token, search, mo, ml)

authorsNewsSearchListExcept ::
  POOL.Pool SQL.Connection ->
  ( News.Handle IO,
    DataTypes.Token,
    Maybe T.Text,
    Maybe DataTypes.Offset,
    Maybe DataTypes.Limit
  ) ->
  EX.ExceptT ErrorTypes.GetNewsError IO [DataTypes.News]
authorsNewsSearchListExcept _ (h, _, Nothing, _, _) = Throw.throwInvalidSearch h "newsSearchListExcept" $ ErrorTypes.InvalidRequest " Not text for searching"
authorsNewsSearchListExcept pool (h, token, Just search, mo, ml) = do
  user <- EX.withExceptT ErrorTypes.GetNewsSearchUserError (LibIO.searchUser pool h token)
  liftIO $ Logger.logInfo (News.hLogHandle h) $ "\n\nRequest with authentication: Get News Search List " <> search <> ", offset = " .< mo <> ", limit = " .< ml
  _ <- EX.withExceptT ErrorTypes.InvalidPermissionGetNews (Lib.checkUserAuthor h user)
  (offset, limit) <- EX.withExceptT ErrorTypes.InvalidOffsetOrLimitGetNews $ OffsetLimit.checkOffsetLimit h mo ml
  dbNews <- authorsNewsSearchListFromDb pool h user search offset limit
  news <- Prelude.mapM (NewsIO.toNews pool h) dbNews
  let toTextNews = T.concat $ map ToText.toText news
  liftIO $ Logger.logDebug (News.hLogHandle h) $ "authorsNewsSearchListExcept: OK! \n" <> toTextNews
  return news

authorsNewsSearchListFromDb ::
  POOL.Pool SQL.Connection ->
  News.Handle IO ->
  DataTypes.User ->
  T.Text ->
  DataTypes.Offset ->
  DataTypes.Limit ->
  EX.ExceptT ErrorTypes.GetNewsError IO [NewsHelpTypes.DbNews]
authorsNewsSearchListFromDb pool h DataTypes.User {..} search DataTypes.Offset {..} DataTypes.Limit {..} = do
  res <-
    liftIO
      ( EXS.try
          ( POOL.withResource pool $ \conn ->
              SQL.query
                conn
                [sql| SELECT news_title, news_created, usr_name, category_id, category_name, news_text, news_images_id, cardinality (news_images_id), news_published , news_id
            FROM news
            INNER JOIN usr ON news.news_author_login = usr.usr_login INNER JOIN category ON news.news_category_id = category.category_id
            where ((news_published = true) or (news_author_login = ? )) and (to_tsvector(news_title) || to_tsvector(usr_name) || to_tsvector(category_name) || to_tsvector(news_text) @@ plainto_tsquery(?))
            ORDER BY news_created DESC
            LIMIT ?  OFFSET ? |]
                (userLogin, search, show limit, show offset)
          ) ::
          IO (Either EXS.SomeException [(T.Text, TIME.Day, T.Text, DataTypes.Id DataTypes.Category, T.Text, T.Text, SQLTypes.PGArray (DataTypes.Id DataTypes.Image), Int, Bool, DataTypes.Id DataTypes.News)])
      )
  case res of
    Left err -> Throw.throwSomeException h "authorsNewsSearchListFromDb" err
    Right newsList -> do
      let dbNews = Prelude.map News.toDbNews newsList
      return dbNews
