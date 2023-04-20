-- | EndPoints.Lib.News.NewsHelpTypes help types for working with news
module EndPoints.Lib.News.NewsHelpTypes where

import qualified Data.Text as T
import qualified Data.Time as TIME
import qualified Types.DataTypes as DataTypes

-- | DbNews - contain data from the database. Makes filtering and sorting easier.
data DbNews = DbNews
  { dbNewsTitle :: DataTypes.Name,
    dbNewsCreated :: TIME.Day,
    dbNewsAuthor :: DataTypes.Name,
    dbNewsCategoryId :: DataTypes.Id DataTypes.Category,
    dbNewsCategoryName :: DataTypes.Name,
    dbNewsText :: T.Text,
    dbNewsImagesId :: [DataTypes.Id DataTypes.Image],
    dbNewsImagesQuantity :: Int,
    dbNewsPublished :: Bool,
    dbNewsId :: DataTypes.Id DataTypes.News
  }
  deriving (Show, Eq)

-- | DbFilter - filtering data to database request
data DbFilter = DbFilter
  { dbFilterDayAt :: DataTypes.DayAt,
    dbFilterDayUntil :: DataTypes.DayUntil,
    dbFilterDaySince :: DataTypes.DaySince,
    dbFilterAuthor :: T.Text,
    dbFilterCategoryId :: Maybe (DataTypes.Id DataTypes.Category),
    dbFilterTitle :: T.Text,
    dbFilterContent :: T.Text
  }
  deriving (Show)
