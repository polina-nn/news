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

module EndPoints.GetNewsList
  ( getNewsList
  , newsList
  ) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as SQL
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified EndPoints.Lib.News.News as News
import qualified EndPoints.Lib.News.NewsHelpTypes as NewsHelpTypes
import qualified EndPoints.Lib.News.NewsIO as NewsIO
import qualified EndPoints.Lib.ToHttpResponse as ToHttpResponse
import qualified EndPoints.Lib.ToText as ToText
import qualified Logger
import qualified News
import Servant (Handler)
import qualified Types.DataTypes as DataTypes
import qualified Types.ErrorTypes as ErrorTypes

getNewsList ::
     News.Handle IO
  -> DataTypes.Db
  -> Maybe DataTypes.DayAt
  -> Maybe DataTypes.DayUntil
  -> Maybe DataTypes.DaySince
  -> Maybe T.Text
  -> Maybe Int
  -> Maybe T.Text
  -> Maybe T.Text
  -> Maybe DataTypes.SortBy
  -> Maybe DataTypes.Offset
  -> Maybe DataTypes.Limit
  -> Handler [DataTypes.News]
getNewsList h DataTypes.Db {..} da du ds ar i t c mSort mo ml =
  (>>=)
    (liftIO $ _newsList (h, filter', mSort, mo, ml))
    ToHttpResponse.toHttpResponse
  where
    filter' :: DataTypes.Filter
    filter' = News.toFilter da du ds ar i t c

newsList ::
     SQL.Connection
  -> ( News.Handle IO
     , DataTypes.Filter
     , Maybe DataTypes.SortBy
     , Maybe DataTypes.Offset
     , Maybe DataTypes.Limit)
  -> IO (Either ErrorTypes.GetNewsError [DataTypes.News])
newsList conn (h, f, mSort, mo, ml) = do
  Logger.logInfo (News.hLogHandle h) $ T.pack "Request: Get News List "
  resAllCheck <- News.checkOffsetLimitFilter (h, f, mo, ml)
  case resAllCheck of
    Left err -> do
      Logger.logError (News.hLogHandle h) $
        T.concat [T.pack "All Check: BAD!  \n"]
      return $ Left err
    Right (offset, limit, dbFiler) -> do
      Logger.logDebug (News.hLogHandle h) $
        T.concat [T.pack "All Check: OK!  \n"]
      res <- newsList' conn offset limit dbFiler >>= News.sortNews h mSort
      news <- Prelude.mapM (NewsIO.toNews conn h) res
      case News.checkErrorsToNews news res of
        (True, news') -> do
          let toTextNews = (T.concat $ map ToText.toText news') :: T.Text
          Logger.logInfo (News.hLogHandle h) $
            T.concat [T.pack "newsList: OK! \n", toTextNews]
          return $ Right news'
        _ ->
          return $
          Left $
          ErrorTypes.GetNewsSQLRequestError $ ErrorTypes.SQLRequestError []

-- | newsList'  get the full list of news if the array is empty, there is no news
newsList' ::
     SQL.Connection
  -> DataTypes.Offset
  -> DataTypes.Limit
  -> NewsHelpTypes.DbFilter
  -> IO [NewsHelpTypes.DbNews]
newsList' conn mo ml f@NewsHelpTypes.DbFilter {..}
  --  category specified in db_filer_category_id
  | isJust dbFilerCategoryId = newsListCategory conn mo ml f
  -- category not specified
  | otherwise = newsListNotCategory conn mo ml f

newsListCategory ::
     SQL.Connection
  -> DataTypes.Offset
  -> DataTypes.Limit
  -> NewsHelpTypes.DbFilter
  -> IO [NewsHelpTypes.DbNews]
newsListCategory conn off lim NewsHelpTypes.DbFilter {..} = do
  res <-
    SQL.query
      conn
      [sql| SELECT news_title, news_created, usr_name, category_path, category_name, news_text, news_images_id, cardinality (news_images_id), news_published
            FROM news
            INNER JOIN usr ON news.news_author_login = usr.usr_login INNER JOIN category ON news.news_category_id = category.category_id
            WHERE (news_published = true) 
            AND (news_created = ? OR news_created < ? OR news_created > ?) 
            AND usr_name LIKE ?
            AND news_title LIKE ?
            AND news_text LIKE ?
            AND news_category_id = ?
            ORDER BY news_created DESC 
            LIMIT ?  OFFSET ?;|]
      ( dbFilerDayAt
      , dbFilerDayUntil
      , dbFilerDaySince
      , dbFilerAuthor
      , dbFilerTitle
      , dbFilerContent
      , newsCat
      , lim
      , off)
  let dbNews = Prelude.map News.toDbNews res
  return dbNews
  where
    newsCat = fromMaybe 0 dbFilerCategoryId

newsListNotCategory ::
     SQL.Connection
  -> DataTypes.Offset
  -> DataTypes.Limit
  -> NewsHelpTypes.DbFilter
  -> IO [NewsHelpTypes.DbNews]
newsListNotCategory conn off lim NewsHelpTypes.DbFilter {..} = do
  res <-
    SQL.query
      conn
      [sql| SELECT news_title, news_created, usr_name, category_path, category_name, news_text, news_images_id, cardinality (news_images_id), news_published
            FROM news
            INNER JOIN usr ON news.news_author_login = usr.usr_login INNER JOIN category ON news.news_category_id = category.category_id
            WHERE (news_published = true) 
            AND (news_created = ? OR news_created < ? OR news_created > ?) 
            AND usr_name LIKE ?
            AND news_title LIKE ?
            AND news_text LIKE ?
            ORDER BY news_created DESC 
            LIMIT ?  OFFSET ?;|]
      ( dbFilerDayAt
      , dbFilerDayUntil
      , dbFilerDaySince
      , dbFilerAuthor
      , dbFilerTitle
      , dbFilerContent
      , lim
      , off)
  let dbNews = Prelude.map News.toDbNews res
  return dbNews
