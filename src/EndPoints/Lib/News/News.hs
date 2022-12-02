{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module EndPoints.Lib.News.News
  ( checkUserOffsetLimitFilter, -- use in EndPoints.GetAuthorsNewsList
    checkOffsetLimitFilter, -- use in EndPoints.GetNewsList
    toDbNews, -- use in EndPoints.GetNewsList,  EndPoints.GetAuthorsNewsList
    toFilter, -- use in EndPoints.GetNewsList,  EndPoints.GetAuthorsNewsList
    sortNews, -- use in EndPoints.GetNewsList,  EndPoints.GetAuthorsNewsList
    checkErrorsToNews, -- use in EndPoints.GetNewsList,  EndPoints.GetAuthorsNewsList
  )
where

import Data.List (sortBy)
import Data.Maybe (isNothing)
import qualified Data.Text as T
import qualified Data.Time as TIME
import qualified Database.PostgreSQL.Simple.Types as SQLTypes
import qualified EndPoints.Lib.Lib as Lib
import qualified EndPoints.Lib.News.NewsHelpTypes as NewsHelpTypes
import qualified EndPoints.Lib.OffsetLimit as OffsetLimit
import qualified Logger
import qualified News
import qualified Types.DataTypes as DataTypes
import qualified Types.ErrorTypes as ErrorTypes

toFilter ::
  Maybe DataTypes.DayAt ->
  Maybe DataTypes.DayUntil ->
  Maybe DataTypes.DaySince ->
  Maybe T.Text ->
  Maybe Int ->
  Maybe T.Text ->
  Maybe T.Text ->
  DataTypes.Filter
toFilter filer_dayAt filer_dayUntil filer_daySince filer_author filer_category_id filer_title filer_content =
  DataTypes.Filter {..}

-- | checkUserOffsetLimitFilter - Check the validity of all data in the request. If ok, then a service DbFilter, otherwise an error
-- the validity of SortBy is checked automatically by servant
checkUserOffsetLimitFilter ::
  ( News.Handle IO,
    DataTypes.User,
    DataTypes.Filter,
    Maybe DataTypes.Offset,
    Maybe DataTypes.Limit
  ) ->
  IO
    ( Either
        ErrorTypes.GetNewsError
        ( DataTypes.Offset,
          DataTypes.Limit,
          NewsHelpTypes.DbFilter
        )
    )
checkUserOffsetLimitFilter (h, user, f, mo, ml) = do
  checkAuthor <- Lib.checkUserAuthor h user
  case checkAuthor of
    Left err -> return $ Left $ ErrorTypes.InvalidPermissionGetNews err
    Right _ -> do
      checkRequest <- OffsetLimit.checkOffsetLimitNews h mo ml
      case checkRequest of
        Left err -> return $ Left err
        Right (offset, limit) ->
          case checkFilter f of
            Left er -> return $ Left er
            Right dbFilter -> return $ Right (offset, limit, dbFilter)

-- | checkOffsetLimitFilter - Check the validity of all data in the request. If ok, then a service DbFilter, otherwise an error
-- the validity of SortBy is checked automatically by servant
checkOffsetLimitFilter ::
  ( News.Handle IO,
    DataTypes.Filter,
    Maybe DataTypes.Offset,
    Maybe DataTypes.Limit
  ) ->
  IO
    ( Either
        ErrorTypes.GetNewsError
        ( DataTypes.Offset,
          DataTypes.Limit,
          NewsHelpTypes.DbFilter
        )
    )
checkOffsetLimitFilter (h, f, mo, ml) = do
  checkRequest <- OffsetLimit.checkOffsetLimitNews h mo ml
  case checkRequest of
    Left err -> return $ Left err
    Right (offset, limit) ->
      case checkFilter f of
        Left er -> return $ Left er
        Right dbFilter -> return $ Right (offset, limit, dbFilter)

-- | checkFilter - Check the values in the filters and send the default values for a convenient request
checkFilter ::
  DataTypes.Filter -> Either ErrorTypes.GetNewsError NewsHelpTypes.DbFilter
checkFilter f@DataTypes.Filter {..} =
  case dayAtOrUntilOrSince f of
    Left err -> Left err
    Right _ ->
      Right
        NewsHelpTypes.DbFilter
          { db_filer_dayAt = dayAt' filer_dayAt,
            db_filer_dayUntil = dayUntil' filer_dayUntil,
            db_filer_daySince =
              daySince' filer_dayAt filer_dayUntil filer_daySince,
            db_filer_author = text' filer_author,
            db_filer_category_id = filer_category_id,
            db_filer_title = text' filer_title,
            db_filer_content = text' filer_content
          }

dayAtOrUntilOrSince ::
  DataTypes.Filter -> Either ErrorTypes.GetNewsError DataTypes.Filter
dayAtOrUntilOrSince fi@DataTypes.Filter {..}
  | isNothing filer_dayAt
      && isNothing filer_dayUntil
      && isNothing filer_daySince =
    Right fi
  | isNothing filer_dayUntil && isNothing filer_daySince = Right fi
  | isNothing filer_dayAt && isNothing filer_daySince = Right fi
  | isNothing filer_dayAt && isNothing filer_dayUntil = Right fi
  | otherwise =
    Left $ ErrorTypes.InvalidFilterGetNews $ ErrorTypes.InvalidRequest []

dayAt' :: Maybe DataTypes.DayAt -> TIME.Day
dayAt' Nothing = TIME.fromGregorian 2000 01 01 --- read "2000 - 01 - 01" :: TIME.Day -- day before dbase create
dayAt' (Just v) = v

dayUntil' :: Maybe DataTypes.DayUntil -> TIME.Day
dayUntil' Nothing = TIME.fromGregorian 2000 01 01 --- read "2000 - 01 - 01" :: TIME.Day --  day before dbase create
dayUntil' (Just v) = v

-- | daySince'  if all Days are nothing, then set the date to the day before the creation of the database so that everything is shown
daySince' ::
  Maybe DataTypes.DayAt ->
  Maybe DataTypes.DayUntil ->
  Maybe DataTypes.DaySince ->
  TIME.Day
daySince' _ _ (Just v) = v
daySince' dayAt dayUntil daySince
  | isNothing dayAt && isNothing dayUntil && isNothing daySince =
    TIME.fromGregorian 2000 01 01
  | otherwise = TIME.fromGregorian 2100 01 01 -- Put a big day after creation

text' :: Maybe T.Text -> T.Text
text' Nothing = T.pack "%"
text' (Just v) = T.concat [T.pack "%", v, T.pack "%"]

sortNews ::
  News.Handle IO ->
  Maybe DataTypes.SortBy ->
  [NewsHelpTypes.DbNews] ->
  IO [NewsHelpTypes.DbNews]
sortNews h Nothing dbnews = do
  Logger.logDebug (News.hLogHandle h) $
    T.pack "sortNews: default by data (latest news is the first) \n"
  return dbnews
sortNews h (Just DataTypes.SortByAuthor) dbnews = do
  Logger.logDebug (News.hLogHandle h) $ T.pack "sortNews: by author name \n"
  let rez =
        sortBy
          ( \x y ->
              compare
                (NewsHelpTypes.db_news_author x)
                (NewsHelpTypes.db_news_author y)
          )
          dbnews
  return rez
sortNews h (Just DataTypes.SortByCategory) dbnews = do
  Logger.logDebug (News.hLogHandle h) $ T.pack "sortNews: by category name \n"
  let rez =
        sortBy
          ( \x y ->
              compare
                (NewsHelpTypes.db_news_category_name x)
                (NewsHelpTypes.db_news_category_name y)
          )
          dbnews
  return rez
sortNews h (Just DataTypes.SortByData) dbnews = do
  Logger.logDebug (News.hLogHandle h) $
    T.pack "sortNews: by data, default by data (latest first) \n"
  return dbnews
sortNews h (Just DataTypes.SortByFoto) dbnews = do
  Logger.logDebug (News.hLogHandle h) $
    T.pack "sortNews: by number of photos \n"
  let rez =
        sortBy
          ( \x y ->
              compare
                (NewsHelpTypes.db_news_images_quantity x)
                (NewsHelpTypes.db_news_images_quantity y)
          )
          dbnews
  return rez

toDbNews ::
  ( T.Text,
    TIME.Day,
    T.Text,
    String,
    T.Text,
    T.Text,
    SQLTypes.PGArray Int,
    Int,
    Bool
  ) ->
  NewsHelpTypes.DbNews
toDbNews (db_news_title, db_news_created, db_news_author, db_news_category_path, db_news_category_name, db_news_text, db_news_images_id', db_news_images_quantity, db_news_published) =
  let db_news_images_id = SQLTypes.fromPGArray db_news_images_id'
   in NewsHelpTypes.DbNews {..}

-- | checkErrorsToNews Count the number of errors in function toNews, if not return (True,  [DataTypes.News])
checkErrorsToNews ::
  [Either ErrorTypes.GetNewsError DataTypes.News] ->
  [NewsHelpTypes.DbNews] ->
  (Bool, [DataTypes.News])
checkErrorsToNews news res =
  (length res == length (helpCheckErrors news), helpCheckErrors news)
  where
    helpCheckErrors ::
      [Either ErrorTypes.GetNewsError DataTypes.News] -> [DataTypes.News]
    helpCheckErrors [] = []
    helpCheckErrors ((Left _) : xs) = helpCheckErrors xs
    helpCheckErrors ((Right a) : xs) = a : helpCheckErrors xs
