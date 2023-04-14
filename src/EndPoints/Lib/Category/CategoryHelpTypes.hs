-- | EndPoints.Lib.CategoryHelpTypes help types for working add and edit fields in Category
module EndPoints.Lib.Category.CategoryHelpTypes where

import qualified Types.DataTypes as DataTypes

data CategorySort = CategorySort
  { categorySortId :: DataTypes.Id,
    categorySortName :: DataTypes.Name,
    categorySortParentId :: DataTypes.ParentId,
    categorySortPath :: DataTypes.Name
  }
  deriving (Show, Eq)
