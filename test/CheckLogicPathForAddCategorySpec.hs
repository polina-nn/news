module CheckLogicPathForAddCategorySpec
  ( spec,
  )
where

import Control.Monad.Identity (Identity (Identity))
import qualified EndPoints.Lib.Category.Category as Category
import Handle (handleSpec)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck ()
import qualified Types.DataTypes as DataTypes
import qualified Types.ErrorTypes as ErrorTypes

spec :: Spec
spec =
  describe
    "Category Check Logic Path For Add Category - OK! Does not contain holes in the numbering "
    $ do
      it "Invalid Logic Path For Add Category : add path 1.1.3 in  empty table" $
        Category.checkLogicPathForAddCategory
          handleSpec
          req1
          (Right myCategoryEmpty)
          `shouldBe` Identity (Left error1)
      it
        "Invalid Logic Path For Add Category : add path 1.1.8, then path 1.1.7 not exists"
        $ Category.checkLogicPathForAddCategory handleSpec req2 (Right myCategory)
          `shouldBe` Identity (Left error1)
      it "Valid Logic Path For Add Category : add path 1 in not empty table" $
        Category.checkLogicPathForAddCategory handleSpec req3 (Right myCategory)
          `shouldBe` Identity (Right (req3, myCategory))
      it "Valid Logic Path For Add Category : add path 1 in empty table" $
        Category.checkLogicPathForAddCategory
          handleSpec
          req3
          (Right myCategoryEmpty)
          `shouldBe` Identity (Right (req3, myCategoryEmpty))
      it
        "Valid Logic Path For Add Category : add path 1.1.7, then path 1.1.6 is exists"
        $ Category.checkLogicPathForAddCategory handleSpec req4 (Right myCategory)
          `shouldBe` Identity (Right (req4, myCategory))
      it
        "Valid Logic Path For Add Category : add path 1.1.3, then path 1.1.3 is exists"
        $ Category.checkLogicPathForAddCategory handleSpec req5 (Right myCategory)
          `shouldBe` Identity (Right (req5, myCategory))
      it "Valid Logic Path For Add Category : add path 3.1, then path 3 is exists" $
        Category.checkLogicPathForAddCategory handleSpec req6 (Right myCategory)
          `shouldBe` Identity (Right (req6, myCategory))

req1 :: DataTypes.CreateCategoryRequest
req1 = DataTypes.CreateCategoryRequest {path = "1.1.3", category = ""}

req2 :: DataTypes.CreateCategoryRequest
req2 = DataTypes.CreateCategoryRequest {path = "1.1.8", category = ""}

req3 :: DataTypes.CreateCategoryRequest
req3 = DataTypes.CreateCategoryRequest {path = "1", category = ""}

req4 :: DataTypes.CreateCategoryRequest
req4 = DataTypes.CreateCategoryRequest {path = "1.1.7", category = ""}

req5 :: DataTypes.CreateCategoryRequest
req5 = DataTypes.CreateCategoryRequest {path = "1.1.3", category = ""}

req6 :: DataTypes.CreateCategoryRequest
req6 = DataTypes.CreateCategoryRequest {path = "3.1", category = ""}

error1 :: ErrorTypes.AddEditCategoryError
error1 = ErrorTypes.InvalidValuePath $ ErrorTypes.InvalidContent []

myCategoryEmpty :: [DataTypes.Category]
myCategoryEmpty = []

myCategory :: [DataTypes.Category]
myCategory =
  [ DataTypes.Category {categoryPath = "1", categoryId = 1, categoryName = "1"},
    DataTypes.Category
      { categoryPath = "1.1",
        categoryId = 3,
        categoryName = "1.1"
      },
    DataTypes.Category
      { categoryPath = "1.1.1",
        categoryId = 4,
        categoryName = "1.1.1"
      },
    DataTypes.Category
      { categoryPath = "1.1.2",
        categoryId = 5,
        categoryName = "1.1.2"
      },
    DataTypes.Category
      { categoryPath = "1.1.3",
        categoryId = 14,
        categoryName = "1.1.3"
      },
    DataTypes.Category
      { categoryPath = "1.1.4",
        categoryId = 15,
        categoryName = "1.1.4"
      },
    DataTypes.Category
      { categoryPath = "1.1.5",
        categoryId = 19,
        categoryName = "1.1.5"
      },
    DataTypes.Category
      { categoryPath = "1.1.6",
        categoryId = 20,
        categoryName = "1.1.6"
      },
    DataTypes.Category
      { categoryPath = "1.2",
        categoryId = 6,
        categoryName = "1.2"
      },
    DataTypes.Category
      { categoryPath = "1.3",
        categoryId = 7,
        categoryName = "1.3"
      },
    DataTypes.Category {categoryPath = "2", categoryId = 2, categoryName = "2"},
    DataTypes.Category
      { categoryPath = "2.1",
        categoryId = 8,
        categoryName = "2.1"
      },
    DataTypes.Category
      { categoryPath = "2.1.2",
        categoryId = 9,
        categoryName = "2.1.2"
      },
    DataTypes.Category
      { categoryPath = "2.1.2.1",
        categoryId = 10,
        categoryName = "2.1.2.1"
      },
    DataTypes.Category
      { categoryPath = "2.1.2.1.1",
        categoryId = 11,
        categoryName = "2.1.2.1.1,"
      },
    DataTypes.Category
      { categoryPath = "2.1.2.1.2",
        categoryId = 16,
        categoryName = "2.1.2.1.2"
      },
    DataTypes.Category
      { categoryPath = "2.2",
        categoryId = 12,
        categoryName = "2.2"
      },
    DataTypes.Category
      { categoryPath = "2.3",
        categoryId = 13,
        categoryName = "2.3"
      },
    DataTypes.Category
      { categoryPath = "2.4",
        categoryId = 17,
        categoryName = "2.4"
      },
    DataTypes.Category
      { categoryPath = "2.5",
        categoryId = 18,
        categoryName = "2.5"
      },
    DataTypes.Category {categoryPath = "3", categoryId = 19, categoryName = "3"},
    DataTypes.Category {categoryPath = "4", categoryId = 20, categoryName = "4"},
    DataTypes.Category {categoryPath = "5", categoryId = 21, categoryName = "5"}
  ]
