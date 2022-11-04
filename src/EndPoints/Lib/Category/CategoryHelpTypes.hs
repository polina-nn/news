{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

-- | EndPoints.Lib.CategoryHelpTypes help types for working add and edit fields in Category
module EndPoints.Lib.Category.CategoryHelpTypes where

import qualified Types.DataTypes as DataTypes

-- | EditCategory - use in logic function for add or edit category
data EditCategory =
  EditCategory
    -- | _id - never change
    { _id :: DataTypes.Id
    -- | new_path - change path when moving categories (when edit or add category)
    , new_path :: DataTypes.Path
    }
  deriving (Show, Eq)

-- | EditCategoryFullReques - full request,
-- based on edit request. Created by toFullReqiest function after checking edit requst and table category in Data Base.
data EditCategoryFullRequest =
  EditCategoryFullRequest
    -- | id' - never change  take from put request  (e.g http://localhost:8080/category/41  id' = 41 )
    { id' :: DataTypes.Id
    -- | cur_path' - path of category with id'
    , cur_path' :: DataTypes.Path
    -- | cur_category' - name of category with id'
    , cur_category' :: DataTypes.Name
    -- | cur_level' - level of category with id'
    , cur_level' :: Level
    -- | new_path' - expected new path for category with id' (take from request, if path not changed new_path' == cur_path' )
    , new_path' :: DataTypes.Path
    -- | new_category' - expected new name for category with id' (take from request, if name not changed new_category' == cur_category' )
    , new_category' :: DataTypes.Name
    -- | cur_level' - level of category new_path'
    , new_level' :: Level
    -- | new_path_stay_after_id' - allow to add a new puff in the right place
    , new_path_stay_after_id' :: DataTypes.Id
    }
  deriving (Show, Eq)

-- | type Level -  path's level (e.g. 1.3.4.45 - level = 4)
type Level = Int
