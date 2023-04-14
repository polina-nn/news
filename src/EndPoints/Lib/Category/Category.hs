module EndPoints.Lib.Category.Category
  ( toCategory,
    toCategorySort,
    toCategoryFromCategorySort,
  )
where

import qualified EndPoints.Lib.Category.CategoryHelpTypes as CategoryHelpTypes
import qualified Types.DataTypes as DataTypes

toCategory :: (Int, DataTypes.Name, Int) -> DataTypes.Category
toCategory (catId, categoryName, parentCatId) =
  DataTypes.Category
    { categoryId = DataTypes.Id {id = catId},
      categoryName = categoryName,
      categoryParentId = DataTypes.ParentId {parentId = parentCatId}
    }

toCategorySort ::
  (Int, DataTypes.Name, Int, DataTypes.Name) -> CategoryHelpTypes.CategorySort
toCategorySort (catSortId, catSortName, catSortParentId, catSortPath) =
  CategoryHelpTypes.CategorySort
    { categorySortId = DataTypes.Id {id = catSortId},
      categorySortName = catSortName,
      categorySortParentId = DataTypes.ParentId {parentId = catSortParentId},
      categorySortPath = catSortPath
    }

toCategoryFromCategorySort :: CategoryHelpTypes.CategorySort -> DataTypes.Category
toCategoryFromCategorySort CategoryHelpTypes.CategorySort {..} =
  DataTypes.Category
    { categoryId = categorySortId,
      categoryName = categorySortName,
      categoryParentId = categorySortParentId
    }
