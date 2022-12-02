{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module СhangePathForEditCategStep7Spec
  ( spec,
  )
where

import qualified EndPoints.Lib.Category.Category as Category
import qualified EndPoints.Lib.Category.CategoryHelpTypes as CategoryHelpTypes
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck ()
import qualified Types.DataTypes as DataTypes

spec :: Spec
spec = describe "Category.changePathsForEditCateg. Vahram Olegovich Sargsyan - Ivanov move one level deeper" $ do
  it "Step1:" $
    Category.step1CurPathWithChildrenToZeroTEST fullReq myCategory `shouldBe` categoryAfterStepOne
  it "Step2:" $
    Category.step2DeleteHoleTEST fullReq myCategory `shouldBe` categoryAfterStepTwo
  it "Step3:" $
    Category.step3MadeHoleTEST fullReq myCategory `shouldBe` categoryAfterStepThree
  it "Step4:" $
    Category.step4ChangeZeroWhithChildrenToNewTEST fullReq myCategory `shouldBe` categoryAfterStepFour
  it "Make Result - Maybe [CategoryHelpTypes.EditCategory]" $
    Category.changePathsForEditCateg fullReq myCategory `shouldBe` editCategoryRequests

-- The fullReq1 value is taken from the CheckLogicPathForEditCategSpec tests, i.e. it passed the validity test
-- Let's put things in order in this database
-- STEP7 Vahram Olegovich Sargsyan - Ivanov move one level deeper
myCategory :: [DataTypes.Category]
myCategory =
  [ DataTypes.Category {category_path = "1", category_id = 11, category_name = "Aнна"},
    DataTypes.Category {category_path = "1.1", category_id = 8, category_name = "Анна Борисовна"},
    DataTypes.Category {category_path = "2", category_id = 2, category_name = "Борис"},
    DataTypes.Category {category_path = "3", category_id = 5, category_name = "Ваграм"},
    DataTypes.Category {category_path = "3.1", category_id = 12, category_name = "Ваграм Ааронович"},
    DataTypes.Category {category_path = "3.2", category_id = 6, category_name = "Ваграм Александрович"},
    DataTypes.Category {category_path = "3.3", category_id = 7, category_name = "Ваграм Олегович"},
    DataTypes.Category {category_path = "3.3.1", category_id = 13, category_name = "Ваграм Олегович Саркисян"},
    DataTypes.Category {category_path = "3.3.2", category_id = 9, category_name = "Ваграм Олегович Саркисян - Иванов"},
    DataTypes.Category {category_path = "3.4", category_id = 1, category_name = "Ваграм Феликсович"},
    DataTypes.Category {category_path = "4", category_id = 3, category_name = "Женя"},
    DataTypes.Category {category_path = "5", category_id = 4, category_name = "Зоя"},
    DataTypes.Category {category_path = "6", category_id = 10, category_name = "Коля"}
  ]

fullReq :: CategoryHelpTypes.EditCategoryFullRequest
fullReq =
  CategoryHelpTypes.EditCategoryFullRequest
    { id' = 9,
      cur_path' = "3.3.2",
      cur_category' = "Ваграм Олегович Саркисян - Иванов",
      cur_level' = 3,
      new_path' = "3.3.1.1",
      new_category' = "Ваграм Олегович Саркисян - Иванов",
      new_level' = 4,
      new_path_stay_after_id' = 13
    }

categoryAfterStepOne :: [DataTypes.Category]
categoryAfterStepOne =
  [ DataTypes.Category {category_path = "0", category_id = 9, category_name = ""},
    DataTypes.Category {category_path = "1", category_id = 11, category_name = ""},
    DataTypes.Category {category_path = "1.1", category_id = 8, category_name = ""},
    DataTypes.Category {category_path = "2", category_id = 2, category_name = ""},
    DataTypes.Category {category_path = "3", category_id = 5, category_name = ""},
    DataTypes.Category {category_path = "3.1", category_id = 12, category_name = ""},
    DataTypes.Category {category_path = "3.2", category_id = 6, category_name = ""},
    DataTypes.Category {category_path = "3.3", category_id = 7, category_name = ""},
    DataTypes.Category {category_path = "3.3.1", category_id = 13, category_name = ""},
    DataTypes.Category {category_path = "3.4", category_id = 1, category_name = ""},
    DataTypes.Category {category_path = "4", category_id = 3, category_name = ""},
    DataTypes.Category {category_path = "5", category_id = 4, category_name = ""},
    DataTypes.Category {category_path = "6", category_id = 10, category_name = ""}
  ]

categoryAfterStepTwo :: [DataTypes.Category]
categoryAfterStepTwo =
  [ DataTypes.Category {category_path = "0", category_id = 9, category_name = ""},
    DataTypes.Category {category_path = "1", category_id = 11, category_name = ""},
    DataTypes.Category {category_path = "1.1", category_id = 8, category_name = ""},
    DataTypes.Category {category_path = "2", category_id = 2, category_name = ""},
    DataTypes.Category {category_path = "3", category_id = 5, category_name = ""},
    DataTypes.Category {category_path = "3.1", category_id = 12, category_name = ""},
    DataTypes.Category {category_path = "3.2", category_id = 6, category_name = ""},
    DataTypes.Category {category_path = "3.3", category_id = 7, category_name = ""},
    DataTypes.Category {category_path = "3.3.1", category_id = 13, category_name = ""},
    DataTypes.Category {category_path = "3.4", category_id = 1, category_name = ""},
    DataTypes.Category {category_path = "4", category_id = 3, category_name = ""},
    DataTypes.Category {category_path = "5", category_id = 4, category_name = ""},
    DataTypes.Category {category_path = "6", category_id = 10, category_name = ""}
  ]

categoryAfterStepThree :: [DataTypes.Category]
categoryAfterStepThree =
  [ DataTypes.Category {category_path = "0", category_id = 9, category_name = ""},
    DataTypes.Category {category_path = "1", category_id = 11, category_name = ""},
    DataTypes.Category {category_path = "1.1", category_id = 8, category_name = ""},
    DataTypes.Category {category_path = "2", category_id = 2, category_name = ""},
    DataTypes.Category {category_path = "3", category_id = 5, category_name = ""},
    DataTypes.Category {category_path = "3.1", category_id = 12, category_name = ""},
    DataTypes.Category {category_path = "3.2", category_id = 6, category_name = ""},
    DataTypes.Category {category_path = "3.3", category_id = 7, category_name = ""},
    DataTypes.Category {category_path = "3.3.1", category_id = 13, category_name = ""},
    DataTypes.Category {category_path = "3.4", category_id = 1, category_name = ""},
    DataTypes.Category {category_path = "4", category_id = 3, category_name = ""},
    DataTypes.Category {category_path = "5", category_id = 4, category_name = ""},
    DataTypes.Category {category_path = "6", category_id = 10, category_name = ""}
  ]

categoryAfterStepFour :: [DataTypes.Category]
categoryAfterStepFour =
  [ DataTypes.Category {category_path = "1", category_id = 11, category_name = ""},
    DataTypes.Category {category_path = "1.1", category_id = 8, category_name = ""},
    DataTypes.Category {category_path = "2", category_id = 2, category_name = ""},
    DataTypes.Category {category_path = "3", category_id = 5, category_name = ""},
    DataTypes.Category {category_path = "3.1", category_id = 12, category_name = ""},
    DataTypes.Category {category_path = "3.2", category_id = 6, category_name = ""},
    DataTypes.Category {category_path = "3.3", category_id = 7, category_name = ""},
    DataTypes.Category {category_path = "3.3.1", category_id = 13, category_name = ""},
    DataTypes.Category {category_path = "3.3.1.1", category_id = 9, category_name = ""},
    DataTypes.Category {category_path = "3.4", category_id = 1, category_name = ""},
    DataTypes.Category {category_path = "4", category_id = 3, category_name = ""},
    DataTypes.Category {category_path = "5", category_id = 4, category_name = ""},
    DataTypes.Category {category_path = "6", category_id = 10, category_name = ""}
  ]

editCategoryRequests :: Maybe [CategoryHelpTypes.EditCategory]
editCategoryRequests =
  Just
    [CategoryHelpTypes.EditCategory {_id = 9, new_path = "3.3.1.1"}]
