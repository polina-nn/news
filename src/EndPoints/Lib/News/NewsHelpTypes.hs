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
    { dbNewsTitle :: DataTypes.Name
    , dbNewsCreated :: TIME.Day
    , dbNewsAuthor :: DataTypes.Name
    , dbNewsCategoryPath :: DataTypes.Path
    , dbNewsCategoryName :: DataTypes.Name -- recursively all categories
    , dbNewsText :: T.Text
    , dbNewsImagesId :: [Int]
    , dbNewsImagesQuantity :: Int
    , dbNewsPublished :: Bool
    }
  deriving (Show, Eq)

-- | DbFilter - filtering data to database request
data DbFilter =
  DbFilter
    { dbFilerDayAt :: TIME.Day
    , dbFilerDayUntil :: TIME.Day
    , dbFilerDaySince :: TIME.Day
    , dbFilerAuthor :: T.Text
    , dbFilerCategoryId :: Maybe Int
    , dbFilerTitle :: T.Text
    , dbFilerContent :: T.Text
    }
  deriving (Show, Eq)
