module EndPoints.Lib.Category.Category
  ( toCategory,
    toCategorySort,
    toCategoryFromCategorySort,
  )
where

import qualified EndPoints.Lib.Category.CategoryHelpTypes as CategoryHelpTypes
import qualified Types.DataTypes as DataTypes

toCategory ::
  (DataTypes.Id, DataTypes.Name, DataTypes.Id) -> DataTypes.Category
toCategory (categoryId, categoryName, categoryParentId) = DataTypes.Category {..}

toCategorySort ::
  (DataTypes.Id, DataTypes.Name, DataTypes.Id, DataTypes.Name) -> CategoryHelpTypes.CategorySort
toCategorySort (categorySortId, categorySortName, categorySortParentId, categorySortPath) = CategoryHelpTypes.CategorySort {..}

toCategoryFromCategorySort :: CategoryHelpTypes.CategorySort -> DataTypes.Category
toCategoryFromCategorySort CategoryHelpTypes.CategorySort {..} =
  DataTypes.Category
    { categoryId = categorySortId,
      categoryName = categorySortName,
      categoryParentId = categorySortParentId
    }
