-- | EndPoints.Lib.CategoryHelpTypes help types for working add and edit fields in Category
module EndPoints.Lib.Category.CategoryHelpTypes where

import qualified Types.DataTypes as DataTypes

data CategorySort = CategorySort
  { categorySortId :: DataTypes.Id DataTypes.Category,
    categorySortName :: DataTypes.Name,
    categorySortParentId :: DataTypes.Id DataTypes.Category,
    categorySortPath :: DataTypes.Name
  }
  deriving (Show, Eq)
