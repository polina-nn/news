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
import qualified EndPoints.Lib.ThrowError as Throw
import qualified EndPoints.Lib.ToHttpResponse as ToHttpResponse
import qualified EndPoints.Lib.ToText as ToText
import Logger (logInfo, (.<))
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
newsSearchList pool (h, search, mo, ml) = EX.runExceptT $ newsSearchListExcept pool (h, search, mo, ml)

newsSearchListExcept ::
  POOL.Pool SQL.Connection ->
  ( News.Handle IO,
    Maybe T.Text,
    Maybe DataTypes.Offset,
    Maybe DataTypes.Limit
  ) ->
  EX.ExceptT ErrorTypes.GetNewsError IO [DataTypes.News]
newsSearchListExcept _ (h, Nothing, _, _) = Throw.throwInvalidSearch h "newsSearchListExcept" $ ErrorTypes.InvalidRequest " Not text for searching"
newsSearchListExcept pool (h, Just search, mo, ml) = do
  liftIO $ Logger.logInfo (News.hLogHandle h) $ "\n\nRequest: Get News Search List " <> search <> ", offset = " .< mo <> ", limit = " .< ml
  (offset, limit) <- EX.withExceptT ErrorTypes.InvalidOffsetOrLimitGetNews $ OffsetLimit.checkOffsetLimit h mo ml
  res <- newsSearchListAtDb pool h search offset limit
  news <- Prelude.mapM (NewsIO.toNews pool h) res
  let toTextNews = T.concat $ map ToText.toText news
  liftIO $ Logger.logInfo (News.hLogHandle h) $ "newsSearchListExcept: OK! \n" <> toTextNews
  return news

newsSearchListAtDb ::
  POOL.Pool SQL.Connection ->
  News.Handle IO ->
  T.Text ->
  DataTypes.Offset ->
  DataTypes.Limit ->
  EX.ExceptT ErrorTypes.GetNewsError IO [NewsHelpTypes.DbNews]
newsSearchListAtDb pool h search DataTypes.Offset {..} DataTypes.Limit {..} = do
  res <-
    liftIO
      ( EXS.try
          ( POOL.withResource pool $ \conn ->
              SQL.query
                conn
                [sql| SELECT news_title, news_created, usr_name, category_id, category_name, news_text, news_images_id, cardinality (news_images_id), news_published, news_id
            FROM news
            INNER JOIN usr ON news.news_author_login = usr.usr_login INNER JOIN category ON news.news_category_id = category.category_id
            where (news_published = true) and (to_tsvector(news_title) || to_tsvector(usr_name) || to_tsvector(category_name) || to_tsvector(news_text) @@ plainto_tsquery(?))
            ORDER BY news_created DESC
            LIMIT ?  OFFSET ? |]
                (search, show limit, show offset)
          ) ::
          IO (Either EXS.SomeException [(T.Text, TIME.Day, T.Text, DataTypes.Id DataTypes.Category, T.Text, T.Text, SQLTypes.PGArray (DataTypes.Id DataTypes.Image), Int, Bool, DataTypes.Id DataTypes.News)])
      )
  case res of
    Left err -> Throw.throwSomeException h "newsSearchListAtDb" err
    Right newsList -> do
      let dbNews = Prelude.map News.toDbNews newsList
      return dbNews
