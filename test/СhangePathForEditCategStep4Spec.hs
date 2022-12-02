{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module СhangePathForEditCategStep4Spec
  ( spec,
  )
where

import qualified EndPoints.Lib.Category.Category as Category
import qualified EndPoints.Lib.Category.CategoryHelpTypes as CategoryHelpTypes
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck ()
import qualified Types.DataTypes as DataTypes

spec :: Spec
spec = describe "Category.changePathsForEditCateg. Put Vagram Aaronovich before Vagram Alexandrovich" $ do
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
-- STEP4 Put Vagram Aaronovich before Vagram Alexandrovich
myCategory :: [DataTypes.Category]
myCategory =
  [ DataTypes.Category {category_path = "1", category_id = 11, category_name = "Aнна"},
    DataTypes.Category {category_path = "2", category_id = 2, category_name = "Борис"},
    DataTypes.Category {category_path = "3", category_id = 5, category_name = "Ваграм"},
    DataTypes.Category {category_path = "3.1", category_id = 6, category_name = "Ваграм Александрович"},
    DataTypes.Category {category_path = "3.2", category_id = 7, category_name = "Ваграм Олегович"},
    DataTypes.Category {category_path = "3.2.1", category_id = 8, category_name = "Анна Борисовна"},
    DataTypes.Category {category_path = "3.2.2", category_id = 9, category_name = "Ваграм Олегович Саркисян - Иванов"},
    DataTypes.Category {category_path = "3.3", category_id = 1, category_name = "Ваграм Феликсович"},
    DataTypes.Category {category_path = "4", category_id = 3, category_name = "Женя"},
    DataTypes.Category {category_path = "5", category_id = 4, category_name = "Зоя"},
    DataTypes.Category {category_path = "6", category_id = 10, category_name = "Коля"},
    DataTypes.Category {category_path = "7", category_id = 12, category_name = "Ваграм Ааронович"},
    DataTypes.Category {category_path = "8", category_id = 13, category_name = "Ваграм Олегович Саркисян"}
  ]

fullReq :: CategoryHelpTypes.EditCategoryFullRequest
fullReq =
  CategoryHelpTypes.EditCategoryFullRequest
    { id' = 12,
      cur_path' = "7",
      cur_category' = "Ваграм Ааронович",
      cur_level' = 1,
      new_path' = "3.1",
      new_category' = "Ваграм Ааронович",
      new_level' = 2,
      new_path_stay_after_id' = 5
    }

categoryAfterStepOne :: [DataTypes.Category]
categoryAfterStepOne =
  [ DataTypes.Category {category_path = "0", category_id = 12, category_name = ""},
    DataTypes.Category {category_path = "1", category_id = 11, category_name = ""},
    DataTypes.Category {category_path = "2", category_id = 2, category_name = ""},
    DataTypes.Category {category_path = "3", category_id = 5, category_name = ""},
    DataTypes.Category {category_path = "3.1", category_id = 6, category_name = ""},
    DataTypes.Category {category_path = "3.2", category_id = 7, category_name = ""},
    DataTypes.Category {category_path = "3.2.1", category_id = 8, category_name = ""},
    DataTypes.Category {category_path = "3.2.2", category_id = 9, category_name = ""},
    DataTypes.Category {category_path = "3.3", category_id = 1, category_name = ""},
    DataTypes.Category {category_path = "4", category_id = 3, category_name = ""},
    DataTypes.Category {category_path = "5", category_id = 4, category_name = ""},
    DataTypes.Category {category_path = "6", category_id = 10, category_name = ""},
    DataTypes.Category {category_path = "8", category_id = 13, category_name = ""}
  ]

categoryAfterStepTwo :: [DataTypes.Category]
categoryAfterStepTwo =
  [ DataTypes.Category {category_path = "0", category_id = 12, category_name = ""},
    DataTypes.Category {category_path = "1", category_id = 11, category_name = ""},
    DataTypes.Category {category_path = "2", category_id = 2, category_name = ""},
    DataTypes.Category {category_path = "3", category_id = 5, category_name = ""},
    DataTypes.Category {category_path = "3.1", category_id = 6, category_name = ""},
    DataTypes.Category {category_path = "3.2", category_id = 7, category_name = ""},
    DataTypes.Category {category_path = "3.2.1", category_id = 8, category_name = ""},
    DataTypes.Category {category_path = "3.2.2", category_id = 9, category_name = ""},
    DataTypes.Category {category_path = "3.3", category_id = 1, category_name = ""},
    DataTypes.Category {category_path = "4", category_id = 3, category_name = ""},
    DataTypes.Category {category_path = "5", category_id = 4, category_name = ""},
    DataTypes.Category {category_path = "6", category_id = 10, category_name = ""},
    DataTypes.Category {category_path = "7", category_id = 13, category_name = ""}
  ]

categoryAfterStepThree :: [DataTypes.Category]
categoryAfterStepThree =
  [ DataTypes.Category {category_path = "0", category_id = 12, category_name = ""},
    DataTypes.Category {category_path = "1", category_id = 11, category_name = ""},
    DataTypes.Category {category_path = "2", category_id = 2, category_name = ""},
    DataTypes.Category {category_path = "3", category_id = 5, category_name = ""},
    DataTypes.Category {category_path = "3.2", category_id = 6, category_name = ""},
    DataTypes.Category {category_path = "3.3", category_id = 7, category_name = ""},
    DataTypes.Category {category_path = "3.3.1", category_id = 8, category_name = ""},
    DataTypes.Category {category_path = "3.3.2", category_id = 9, category_name = ""},
    DataTypes.Category {category_path = "3.4", category_id = 1, category_name = ""},
    DataTypes.Category {category_path = "4", category_id = 3, category_name = ""},
    DataTypes.Category {category_path = "5", category_id = 4, category_name = ""},
    DataTypes.Category {category_path = "6", category_id = 10, category_name = ""},
    DataTypes.Category {category_path = "7", category_id = 13, category_name = ""}
  ]

categoryAfterStepFour :: [DataTypes.Category]
categoryAfterStepFour =
  [ DataTypes.Category {category_path = "1", category_id = 11, category_name = ""},
    DataTypes.Category {category_path = "2", category_id = 2, category_name = ""},
    DataTypes.Category {category_path = "3", category_id = 5, category_name = ""},
    DataTypes.Category {category_path = "3.1", category_id = 12, category_name = ""},
    DataTypes.Category {category_path = "3.2", category_id = 6, category_name = ""},
    DataTypes.Category {category_path = "3.3", category_id = 7, category_name = ""},
    DataTypes.Category {category_path = "3.3.1", category_id = 8, category_name = ""},
    DataTypes.Category {category_path = "3.3.2", category_id = 9, category_name = ""},
    DataTypes.Category {category_path = "3.4", category_id = 1, category_name = ""},
    DataTypes.Category {category_path = "4", category_id = 3, category_name = ""},
    DataTypes.Category {category_path = "5", category_id = 4, category_name = ""},
    DataTypes.Category {category_path = "6", category_id = 10, category_name = ""},
    DataTypes.Category {category_path = "7", category_id = 13, category_name = ""}
  ]

editCategoryRequests :: Maybe [CategoryHelpTypes.EditCategory]
editCategoryRequests =
  Just
    [ CategoryHelpTypes.EditCategory {_id = 1, new_path = "3.4"},
      CategoryHelpTypes.EditCategory {_id = 6, new_path = "3.2"},
      CategoryHelpTypes.EditCategory {_id = 7, new_path = "3.3"},
      CategoryHelpTypes.EditCategory {_id = 8, new_path = "3.3.1"},
      CategoryHelpTypes.EditCategory {_id = 9, new_path = "3.3.2"},
      CategoryHelpTypes.EditCategory {_id = 12, new_path = "3.1"},
      CategoryHelpTypes.EditCategory {_id = 13, new_path = "7"}
    ]
