module EndPoints.Lib.News.News
  ( toDbNews,
    toFilter,
    sortNews,
    checkFilter,
  )
where

import Control.Monad.Trans.Class (MonadTrans (lift))
import qualified Control.Monad.Trans.Except as EX
import Data.List (sortBy)
import qualified Data.Maybe as M
import qualified Data.Text as T
import qualified Data.Time as TIME
import qualified Database.PostgreSQL.Simple.Types as SQLTypes
import qualified EndPoints.Lib.News.NewsHelpTypes as NewsHelpTypes
import qualified Logger
import qualified News
import qualified Types.DataTypes as DataTypes
import qualified Types.ErrorTypes as ErrorTypes

-- | remotePast - time before database creation (use in date filtering)
remotePast :: TIME.Day
remotePast = TIME.fromGregorian 2000 01 01

-- | farFuture - far future  (use in date filtering)
farFuture :: TIME.Day
farFuture = TIME.fromGregorian 2100 01 01

toFilter ::
  Maybe DataTypes.DayAt ->
  Maybe DataTypes.DayUntil ->
  Maybe DataTypes.DaySince ->
  Maybe T.Text ->
  Maybe (DataTypes.Id DataTypes.Category) ->
  Maybe T.Text ->
  Maybe T.Text ->
  DataTypes.Filter
toFilter filterDayAt filterDayUntil filterDaySince filterAuthor filterCategoryId filterTitle filterContent =
  DataTypes.Filter {..}

-- | checkFilter - Check the values in the filters and send the default values for sql request
checkFilter ::
  Monad m =>
  DataTypes.Filter ->
  EX.ExceptT ErrorTypes.GetNewsError m NewsHelpTypes.DbFilter
checkFilter f@DataTypes.Filter {..} = do
  _ <- checkDayFilter f
  return $
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
  Monad m =>
  DataTypes.Filter ->
  EX.ExceptT ErrorTypes.GetNewsError m DataTypes.Filter
checkDayFilter fi@DataTypes.Filter {..}
  | M.isNothing filterDayAt && M.isNothing filterDayUntil && M.isNothing filterDaySince = return fi
  | M.isNothing filterDayUntil && M.isNothing filterDaySince = return fi
  | M.isNothing filterDayAt && M.isNothing filterDaySince = return fi
  | M.isNothing filterDayAt && M.isNothing filterDayUntil = return fi
  | otherwise = EX.throwE $ ErrorTypes.InvalidFilterGetNews $ ErrorTypes.InvalidRequest []

-- | dayAt -- return value for part "AND (news_created = dbFilterDayAt OR news_created < dbFilterDayUntil OR news_created > dbFilterDaySince)"  at select request
-- if filter dbFilterDayAt is not specified, use day before data base created (remotePast)
dayAt :: Maybe DataTypes.DayAt -> DataTypes.DayAt
dayAt = M.fromMaybe (DataTypes.DayAt {dayAt = remotePast})

-- | dayUntil -- return value for part "AND (news_created = dbFilterDayAt OR news_created < dbFilterDayUntil OR news_created > dbFilterDaySince)"  at select request
-- if filter dbFilterDayUntil is not specified, use day before data base created (remotePast)
dayUntil :: Maybe DataTypes.DayUntil -> DataTypes.DayUntil
dayUntil = M.fromMaybe (DataTypes.DayUntil {dayUntil = remotePast})

-- | daySince  if all "Days" are nothing, then set the date to the day before the creation of the database (remotePast) so that everything is shown, else use a big day after creation (farFuture)
daySince ::
  Maybe DataTypes.DayAt ->
  Maybe DataTypes.DayUntil ->
  Maybe DataTypes.DaySince ->
  DataTypes.DaySince
daySince _ _ (Just v) = v
daySince dayAt' dayUntil' daySince'
  | M.isNothing dayAt' && M.isNothing dayUntil' && M.isNothing daySince' = (DataTypes.DaySince {daySince = remotePast})
  | otherwise = (DataTypes.DaySince {daySince = farFuture})

-- textLike - return value (instead of "?" ) for part  "AND news_title LIKE ?" at select request
-- LIKE pattern matching always covers the entire string. Therefore, if it's desired to match a sequence anywhere within a string, the pattern must start and end with a percent sign.
textLike :: Maybe T.Text -> T.Text
textLike Nothing = "%"
textLike (Just v) = "%" <> v <> "%"

sortNews ::
  Monad m =>
  News.Handle m ->
  Maybe DataTypes.SortBy ->
  [NewsHelpTypes.DbNews] ->
  EX.ExceptT ErrorTypes.GetNewsError m [NewsHelpTypes.DbNews]
sortNews h Nothing dbNews = do
  lift $ Logger.logDebug (News.hLogHandle h) "sortNews: default by data (latest news is the first)"
  return dbNews
sortNews h (Just DataTypes.SortByAuthor) dbNews = do
  lift $ Logger.logDebug (News.hLogHandle h) "sortNews: by author name "
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
  lift $ Logger.logDebug (News.hLogHandle h) "sortNews: by category name "
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
  lift $ Logger.logDebug (News.hLogHandle h) "sortNews: by data, default by data (latest first)"
  return dbNews
sortNews h (Just DataTypes.SortByPhoto) dbNews = do
  lift $ Logger.logDebug (News.hLogHandle h) "sortNews: by number of photos"
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
    DataTypes.Id DataTypes.Category,
    T.Text,
    T.Text,
    SQLTypes.PGArray (DataTypes.Id DataTypes.Image),
    Int,
    Bool,
    DataTypes.Id DataTypes.News
  ) ->
  NewsHelpTypes.DbNews
toDbNews (dbNewsTitle, dbNewsCreated, dbNewsAuthor, dbNewsCategoryId, dbNewsCategoryName, dbNewsText, dbNewsImagesId', dbNewsImagesQuantity, dbNewsPublished, dbNewsId) =
  let dbNewsImagesId = SQLTypes.fromPGArray dbNewsImagesId'
   in NewsHelpTypes.DbNews {..}
