{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module EndPoints.GetNewsSearchList
  ( getNewsSearchList
  , newsSearchList
  ) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as SQL
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified EndPoints.Lib.News.News as News
import qualified EndPoints.Lib.News.NewsHelpTypes as NewsHelpTypes
import qualified EndPoints.Lib.News.NewsIO as NewsIO
import qualified EndPoints.Lib.OffsetLimit as OffsetLimit
import qualified EndPoints.Lib.ToHttpResponse as ToHttpResponse
import qualified EndPoints.Lib.ToText as ToText
import qualified Logger
import qualified News
import Servant (Handler)
import qualified Types.DataTypes as DataTypes
import qualified Types.ErrorTypes as ErrorTypes

getNewsSearchList ::
     News.Handle IO
  -> DataTypes.Db
  -> Maybe T.Text
  -> Maybe DataTypes.Offset
  -> Maybe DataTypes.Limit
  -> Handler [DataTypes.News]
getNewsSearchList h DataTypes.Db {..} mSearch mo ml =
  (>>=)
    (liftIO $ _newsSearchList (h, mSearch, mo, ml))
    ToHttpResponse.toHttpResponse

newsSearchList ::
     SQL.Connection
  -> ( News.Handle IO
     , Maybe T.Text
     , Maybe DataTypes.Offset
     , Maybe DataTypes.Limit)
  -> IO (Either ErrorTypes.GetNewsError [DataTypes.News])
newsSearchList _ (h, Nothing, _, _) = do
  Logger.logError (News.hLogHandle h) $
    T.pack $
    show $
    ErrorTypes.InvalidSearchGetNews $
    ErrorTypes.InvalidRequest "newsSearchList: BAD! Not text for searching \n"
  return $ Left $ ErrorTypes.InvalidSearchGetNews $ ErrorTypes.InvalidRequest []
newsSearchList conn (h, Just search, mo, ml) = do
  Logger.logInfo (News.hLogHandle h) $ T.pack "Request: Get News Search List "
  checkRequest <- OffsetLimit.checkOffsetLimitNews h mo ml
  case checkRequest of
    Left err -> return $ Left err
    Right (offset, limit) -> do
      res <- newsSearchList' conn h search offset limit
      news <- Prelude.mapM (NewsIO.toNews conn h) res
      case News.checkErrorsToNews news res of
        (True, news') -> do
          let toTextNews = (T.concat $ map ToText.toText news') :: T.Text
          Logger.logInfo (News.hLogHandle h) $
            T.concat [T.pack "authorsNewsSearchList: OK! \n", toTextNews]
          return $ Right news'
        _ ->
          return $
          Left $
          ErrorTypes.GetNewsSQLRequestError $ ErrorTypes.SQLRequestError []

newsSearchList' ::
     SQL.Connection
  -> News.Handle IO
  -> T.Text
  -> DataTypes.Offset
  -> DataTypes.Limit
  -> IO [NewsHelpTypes.DbNews]
newsSearchList' conn _ search off lim = do
  res <-
    SQL.query
      conn
      [sql| SELECT news_title, news_created, usr_name, category_path, category_name, news_text, news_images_id, cardinality (news_images_id), news_published
            FROM news
            INNER JOIN usr ON news.news_author_login = usr.usr_login INNER JOIN category ON news.news_category_id = category.category_id
            where (news_published = true) and (to_tsvector(news_title) || to_tsvector(usr_name) || to_tsvector(category_name) || to_tsvector(news_text) @@ plainto_tsquery(?))
            ORDER BY news_created DESC 
            LIMIT ?  OFFSET ? |]
      (search, show lim, show off)
  print res
  let dbNews = Prelude.map News.toDbNews res
  return dbNews
