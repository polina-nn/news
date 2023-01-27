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
toFilter filterDayAt filterDayUntil filterDaySince filterAuthor filterCategoryId filterTitle filterContent =
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
            Right dbFilter ->
              return $ Right (offset, limit, dbFilter)

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

-- | checkFilter - Check the values in the filters and send the default values for sql request
checkFilter ::
  DataTypes.Filter -> Either ErrorTypes.GetNewsError NewsHelpTypes.DbFilter
checkFilter f@DataTypes.Filter {..} =
  case checkDayFilter f of
    Left err -> Left err
    Right _ ->
      Right
        NewsHelpTypes.DbFilter
          { dbFilterDayAt = dayAt filterDayAt,
            dbFilterDayUntil = dayUntil filterDayUntil,
            dbFilterDaySince = daySince filterDayAt filterDayUntil filterDaySince,
            dbFilterAuthor = textLike filterAuthor,
            dbFilterCategoryId = filterCategoryId,
            dbFilterTitle = textLike filterTitle,
            dbFilterContent = textLike filterContent
          }

-- | checkDayFilter - no more than one date filter in a request
checkDayFilter ::
  DataTypes.Filter -> Either ErrorTypes.GetNewsError DataTypes.Filter
checkDayFilter fi@DataTypes.Filter {..}
  | isNothing filterDayAt && isNothing filterDayUntil && isNothing filterDaySince =
    Right fi
  | isNothing filterDayUntil && isNothing filterDaySince = Right fi
  | isNothing filterDayAt && isNothing filterDaySince = Right fi
  | isNothing filterDayAt && isNothing filterDayUntil = Right fi
  | otherwise =
    Left $ ErrorTypes.InvalidFilterGetNews $ ErrorTypes.InvalidRequest []

-- | dayAt -- return value for part "AND (news_created = dbFilterDayAt OR news_created < dbFilterDayUntil OR news_created > dbFilterDaySince)"  at select request
-- if filter dbFilterDayAt is not specified, use day before data base created ("2000 - 01 - 01")
dayAt :: Maybe DataTypes.DayAt -> TIME.Day
dayAt Nothing = TIME.fromGregorian 2000 01 01
dayAt (Just v) = v

-- | dayUntil -- return value for part "AND (news_created = dbFilterDayAt OR news_created < dbFilterDayUntil OR news_created > dbFilterDaySince)"  at select request
-- if filter dbFilterDayUntil is not specified, use day before data base created ("2000 - 01 - 01")
dayUntil :: Maybe DataTypes.DayUntil -> TIME.Day
dayUntil Nothing = TIME.fromGregorian 2000 01 01
dayUntil (Just v) = v

-- | daySince  if all "Days" are nothing, then set the date to the day before the creation of the database ("2000 - 01 - 01") so that everything is shown, else use a big day after creation ("2100-01-01")
daySince ::
  Maybe DataTypes.DayAt ->
  Maybe DataTypes.DayUntil ->
  Maybe DataTypes.DaySince ->
  TIME.Day
daySince _ _ (Just v) = v
daySince dayAt' dayUntil' daySince'
  | isNothing dayAt' && isNothing dayUntil' && isNothing daySince' =
    TIME.fromGregorian 2000 01 01
  | otherwise = TIME.fromGregorian 2100 01 01

-- textLike - return value (instead of "?" ) for part  "AND news_title LIKE ?" at select request
-- LIKE pattern matching always covers the entire string. Therefore, if it's desired to match a sequence anywhere within a string, the pattern must start and end with a percent sign.
textLike :: Maybe T.Text -> T.Text
textLike Nothing = "%"
textLike (Just v) = T.concat ["%", v, "%"]

sortNews ::
  News.Handle IO ->
  Maybe DataTypes.SortBy ->
  [NewsHelpTypes.DbNews] ->
  IO [NewsHelpTypes.DbNews]
sortNews h Nothing dbNews = do
  Logger.logDebug (News.hLogHandle h) "sortNews: default by data (latest news is the first)"
  return dbNews
sortNews h (Just DataTypes.SortByAuthor) dbNews = do
  Logger.logDebug (News.hLogHandle h) "sortNews: by author name "
  let rez =
        sortBy
          ( \x y ->
              compare
                (NewsHelpTypes.dbNewsAuthor x)
                (NewsHelpTypes.dbNewsAuthor y)
          )
          dbNews
  return rez
sortNews h (Just DataTypes.SortByCategory) dbNews = do
  Logger.logDebug (News.hLogHandle h) "sortNews: by category name "
  let rez =
        sortBy
          ( \x y ->
              compare
                (NewsHelpTypes.dbNewsCategoryName x)
                (NewsHelpTypes.dbNewsCategoryName y)
          )
          dbNews
  return rez
sortNews h (Just DataTypes.SortByData) dbNews = do
  Logger.logDebug (News.hLogHandle h) "sortNews: by data, default by data (latest first)"
  return dbNews
sortNews h (Just DataTypes.SortByPhoto) dbNews = do
  Logger.logDebug (News.hLogHandle h) "sortNews: by number of photos"
  let rez =
        sortBy
          ( \x y ->
              compare
                (NewsHelpTypes.dbNewsImagesQuantity x)
                (NewsHelpTypes.dbNewsImagesQuantity y)
          )
          dbNews
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
toDbNews (dbNewsTitle, dbNewsCreated, dbNewsAuthor, dbNewsCategoryPath, dbNewsCategoryName, dbNewsText, dbNewsImagesId', dbNewsImagesQuantity, dbNewsPublished) =
  let dbNewsImagesId = SQLTypes.fromPGArray dbNewsImagesId'
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
