-- | EndPoints.Lib.CategoryHelpTypes help types for working add and edit fields in Category
module EndPoints.Lib.Category.CategoryHelpTypes where

import qualified Types.DataTypes as DataTypes

data CategorySort = CategorySort
  { categorySortId :: DataTypes.Id DataTypes.CategoryId,
    categorySortName :: DataTypes.Name,
    categorySortParentId :: DataTypes.Id DataTypes.CategoryId,
    categorySortPath :: DataTypes.Name
  }
  deriving (Show, Eq)
