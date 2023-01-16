-- | EndPoints.Lib.CategoryHelpTypes help types for working add and edit fields in Category
module EndPoints.Lib.Category.CategoryHelpTypes where

import qualified Types.DataTypes as DataTypes

-- | EditCategory - use in logic function for add or edit category
data EditCategory = EditCategory
  { -- | _id - never change
    _id :: DataTypes.Id,
    -- | newPath - change path when moving categories (when edit or add category)
    newPath :: DataTypes.Path
  }
  deriving (Show, Eq)

-- | EditCategoryFullRequest - full request,
-- based on edit request. Created by toFullRequest function after checking edit request and table category in Data Base.
data EditCategoryFullRequest = EditCategoryFullRequest
  { -- | id' - never change  take from put request  (e.g http://localhost:8080/category/41  id' = 41 )
    id' :: DataTypes.Id,
    -- | cur_path' - path of category with id'
    curPath' :: DataTypes.Path,
    -- | cur_category' - name of category with id'
    curCategory' :: DataTypes.Name,
    -- | cur_level' - level of category with id'
    curLevel' :: Level,
    -- | new_path' - expected new path for category with id' (take from request, if path not changed new_path' == cur_path' )
    newPath' :: DataTypes.Path,
    -- | new_category' - expected new name for category with id' (take from request, if name not changed new_category' == cur_category' )
    newCategory' :: DataTypes.Name,
    -- | cur_level' - level of category new_path'
    newLevel' :: Level,
    -- | new_path_stay_after_id' - allow to add a new puff in the right place
    newPathStayAfterId' :: DataTypes.Id
  }
  deriving (Show, Eq)

-- | type Level -  path's level (e.g. 1.3.4.45 - level = 4)
type Level = Int
