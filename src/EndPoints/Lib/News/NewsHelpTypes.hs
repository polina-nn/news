{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

-- | EndPoints.Lib.News.NewsHelpTypes help types for working with news
module EndPoints.Lib.News.NewsHelpTypes where

import qualified Data.Text as T
import qualified Data.Time as TIME
import qualified Types.DataTypes as DataTypes

-- | DbNews - contain data from the database. Makes filtering and sorting easier.
data DbNews =
  DbNews
    { db_news_title :: DataTypes.Name
    , db_news_created :: TIME.Day
    , db_news_author :: DataTypes.Name
    , db_news_category_path :: DataTypes.Path
    , db_news_category_name :: DataTypes.Name -- recursively all categories
    , db_news_text :: T.Text
    , db_news_images_id :: [Int]
    , db_news_images_quantity :: Int
    , db_news_published :: Bool
    }
  deriving (Show, Eq)

-- | DbFilter - filtering data to database request
data DbFilter =
  DbFilter
    { db_filer_dayAt :: TIME.Day
    , db_filer_dayUntil :: TIME.Day
    , db_filer_daySince :: TIME.Day
    , db_filer_author :: T.Text
    , db_filer_category_id :: Maybe Int
    , db_filer_title :: T.Text
    , db_filer_content :: T.Text
    }
  deriving (Show, Eq)
