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
    dbNewsCategoryPath :: DataTypes.Path,
    dbNewsCategoryName :: DataTypes.Name, -- recursively all categories
    dbNewsText :: T.Text,
    dbNewsImagesId :: [Int],
    dbNewsImagesQuantity :: Int,
    dbNewsPublished :: Bool
  }
  deriving (Show, Eq)

-- | DbFilter - filtering data to database request
data DbFilter = DbFilter
  { dbFilterDayAt :: TIME.Day,
    dbFilterDayUntil :: TIME.Day,
    dbFilterDaySince :: TIME.Day,
    dbFilterAuthor :: T.Text,
    dbFilterCategoryId :: Maybe Int,
    dbFilterTitle :: T.Text,
    dbFilterContent :: T.Text
  }
  deriving (Show, Eq)
