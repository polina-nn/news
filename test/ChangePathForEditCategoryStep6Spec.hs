module ChangePathForEditCategoryStep6Spec
  ( spec,
  )
where

import qualified EndPoints.Lib.Category.Category as Category
import qualified EndPoints.Lib.Category.CategoryHelpTypes as CategoryHelpTypes
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck ()
import qualified Types.DataTypes as DataTypes

spec :: Spec
spec =
  describe
    "Category.changePathsForEditCategory. Move Vahram Oleg Sargsyan one level down after Vahram Olegovich"
    $ do
      it "Step1:" $
        Category.step1CurPathWithChildrenToZeroTEST fullReq myCategory
          `shouldBe` categoryAfterStepOne
      it "Step2:" $
        Category.step2DeleteHoleTEST fullReq myCategory
          `shouldBe` categoryAfterStepTwo
      it "Step3:" $
        Category.step3MadeHoleTEST fullReq myCategory
          `shouldBe` categoryAfterStepThree
      it "Step4:" $
        Category.step4ChangeZeroWithChildrenToNewTEST fullReq myCategory
          `shouldBe` categoryAfterStepFour
      it "Make Result - Maybe [CategoryHelpTypes.EditCategory]" $
        Category.changePathsForEditCategory fullReq myCategory
          `shouldBe` editCategoryRequests

-- The fullReq1 value is taken from the CheckLogicPathForEditCategSpec tests, i.e. it passed the validity test
-- Let's put things in order in this database
-- STEP6 Move Vahram Oleg Sargsyan one level down after Vahram Olegovich
myCategory :: [DataTypes.Category]
myCategory =
  [ DataTypes.Category
      { categoryPath = "1",
        categoryId = 11,
        categoryName = "Aнна"
      },
    DataTypes.Category
      { categoryPath = "1.1",
        categoryId = 8,
        categoryName = "Анна Борисовна"
      },
    DataTypes.Category
      { categoryPath = "2",
        categoryId = 2,
        categoryName = "Борис"
      },
    DataTypes.Category
      { categoryPath = "3",
        categoryId = 5,
        categoryName = "Ваграм"
      },
    DataTypes.Category
      { categoryPath = "3.1",
        categoryId = 12,
        categoryName = "Ваграм Ааронович"
      },
    DataTypes.Category
      { categoryPath = "3.2",
        categoryId = 6,
        categoryName = "Ваграм Александрович"
      },
    DataTypes.Category
      { categoryPath = "3.3",
        categoryId = 7,
        categoryName = "Ваграм Олегович"
      },
    DataTypes.Category
      { categoryPath = "3.3.1",
        categoryId = 9,
        categoryName = "Ваграм Олегович Саркисян - Иванов"
      },
    DataTypes.Category
      { categoryPath = "3.4",
        categoryId = 1,
        categoryName = "Ваграм Феликсович"
      },
    DataTypes.Category
      { categoryPath = "4",
        categoryId = 3,
        categoryName = "Женя"
      },
    DataTypes.Category
      { categoryPath = "5",
        categoryId = 4,
        categoryName = "Зоя"
      },
    DataTypes.Category
      { categoryPath = "6",
        categoryId = 10,
        categoryName = "Коля"
      },
    DataTypes.Category
      { categoryPath = "7",
        categoryId = 13,
        categoryName = "Ваграм Олегович Саркисян"
      }
  ]

fullReq :: CategoryHelpTypes.EditCategoryFullRequest
fullReq =
  CategoryHelpTypes.EditCategoryFullRequest
    { id' = 13,
      curPath' = "7",
      curCategory' = "Ваграм Олегович Саркисян",
      curLevel' = 1,
      newPath' = "3.3.1",
      newCategory' = "Ваграм Олегович Саркисян",
      newLevel' = 3,
      newPathStayAfterId' = 7
    }

categoryAfterStepOne :: [DataTypes.Category]
categoryAfterStepOne =
  [ DataTypes.Category {categoryPath = "0", categoryId = 13, categoryName = ""},
    DataTypes.Category {categoryPath = "1", categoryId = 11, categoryName = ""},
    DataTypes.Category {categoryPath = "1.1", categoryId = 8, categoryName = ""},
    DataTypes.Category {categoryPath = "2", categoryId = 2, categoryName = ""},
    DataTypes.Category {categoryPath = "3", categoryId = 5, categoryName = ""},
    DataTypes.Category
      { categoryPath = "3.1",
        categoryId = 12,
        categoryName = ""
      },
    DataTypes.Category {categoryPath = "3.2", categoryId = 6, categoryName = ""},
    DataTypes.Category {categoryPath = "3.3", categoryId = 7, categoryName = ""},
    DataTypes.Category
      { categoryPath = "3.3.1",
        categoryId = 9,
        categoryName = ""
      },
    DataTypes.Category {categoryPath = "3.4", categoryId = 1, categoryName = ""},
    DataTypes.Category {categoryPath = "4", categoryId = 3, categoryName = ""},
    DataTypes.Category {categoryPath = "5", categoryId = 4, categoryName = ""},
    DataTypes.Category {categoryPath = "6", categoryId = 10, categoryName = ""}
  ]

categoryAfterStepTwo :: [DataTypes.Category]
categoryAfterStepTwo =
  [ DataTypes.Category {categoryPath = "0", categoryId = 13, categoryName = ""},
    DataTypes.Category {categoryPath = "1", categoryId = 11, categoryName = ""},
    DataTypes.Category {categoryPath = "1.1", categoryId = 8, categoryName = ""},
    DataTypes.Category {categoryPath = "2", categoryId = 2, categoryName = ""},
    DataTypes.Category {categoryPath = "3", categoryId = 5, categoryName = ""},
    DataTypes.Category
      { categoryPath = "3.1",
        categoryId = 12,
        categoryName = ""
      },
    DataTypes.Category {categoryPath = "3.2", categoryId = 6, categoryName = ""},
    DataTypes.Category {categoryPath = "3.3", categoryId = 7, categoryName = ""},
    DataTypes.Category
      { categoryPath = "3.3.1",
        categoryId = 9,
        categoryName = ""
      },
    DataTypes.Category {categoryPath = "3.4", categoryId = 1, categoryName = ""},
    DataTypes.Category {categoryPath = "4", categoryId = 3, categoryName = ""},
    DataTypes.Category {categoryPath = "5", categoryId = 4, categoryName = ""},
    DataTypes.Category {categoryPath = "6", categoryId = 10, categoryName = ""}
  ]

categoryAfterStepThree :: [DataTypes.Category]
categoryAfterStepThree =
  [ DataTypes.Category {categoryPath = "0", categoryId = 13, categoryName = ""},
    DataTypes.Category {categoryPath = "1", categoryId = 11, categoryName = ""},
    DataTypes.Category {categoryPath = "1.1", categoryId = 8, categoryName = ""},
    DataTypes.Category {categoryPath = "2", categoryId = 2, categoryName = ""},
    DataTypes.Category {categoryPath = "3", categoryId = 5, categoryName = ""},
    DataTypes.Category
      { categoryPath = "3.1",
        categoryId = 12,
        categoryName = ""
      },
    DataTypes.Category {categoryPath = "3.2", categoryId = 6, categoryName = ""},
    DataTypes.Category {categoryPath = "3.3", categoryId = 7, categoryName = ""},
    DataTypes.Category
      { categoryPath = "3.3.2",
        categoryId = 9,
        categoryName = ""
      },
    DataTypes.Category {categoryPath = "3.4", categoryId = 1, categoryName = ""},
    DataTypes.Category {categoryPath = "4", categoryId = 3, categoryName = ""},
    DataTypes.Category {categoryPath = "5", categoryId = 4, categoryName = ""},
    DataTypes.Category {categoryPath = "6", categoryId = 10, categoryName = ""}
  ]

categoryAfterStepFour :: [DataTypes.Category]
categoryAfterStepFour =
  [ DataTypes.Category {categoryPath = "1", categoryId = 11, categoryName = ""},
    DataTypes.Category {categoryPath = "1.1", categoryId = 8, categoryName = ""},
    DataTypes.Category {categoryPath = "2", categoryId = 2, categoryName = ""},
    DataTypes.Category {categoryPath = "3", categoryId = 5, categoryName = ""},
    DataTypes.Category
      { categoryPath = "3.1",
        categoryId = 12,
        categoryName = ""
      },
    DataTypes.Category {categoryPath = "3.2", categoryId = 6, categoryName = ""},
    DataTypes.Category {categoryPath = "3.3", categoryId = 7, categoryName = ""},
    DataTypes.Category
      { categoryPath = "3.3.1",
        categoryId = 13,
        categoryName = ""
      },
    DataTypes.Category
      { categoryPath = "3.3.2",
        categoryId = 9,
        categoryName = ""
      },
    DataTypes.Category {categoryPath = "3.4", categoryId = 1, categoryName = ""},
    DataTypes.Category {categoryPath = "4", categoryId = 3, categoryName = ""},
    DataTypes.Category {categoryPath = "5", categoryId = 4, categoryName = ""},
    DataTypes.Category {categoryPath = "6", categoryId = 10, categoryName = ""}
  ]

editCategoryRequests :: Maybe [CategoryHelpTypes.EditCategory]
editCategoryRequests =
  Just
    [ CategoryHelpTypes.EditCategory {permanentId = 9, newPath = "3.3.2"},
      CategoryHelpTypes.EditCategory {permanentId = 13, newPath = "3.3.1"}
    ]
