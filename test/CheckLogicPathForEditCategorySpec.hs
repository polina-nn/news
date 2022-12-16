{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module CheckLogicPathForEditCategorySpec
  ( spec
  ) where

import Control.Monad.Identity (Identity(Identity))
import qualified EndPoints.Lib.Category.Category as Category
import qualified EndPoints.Lib.Category.CategoryHelpTypes as CategoryHelpTypes
import Handle (handleSpec)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck ()
import qualified Types.DataTypes as DataTypes
import qualified Types.ErrorTypes as ErrorTypes

spec :: Spec
spec =
  describe
    "Check Logic Path For Edit Category - if OK! Return Right (EditCategoryFullRequest, [DataTypes.Category]), else Left error  " $ do
    it
      "Step1 Valid Logic Path For Edit Category : edit cur_path' = 1 to newPath= 3" $
      Category.checkLogicPathForEditCategory
        handleSpec
        1
        req1
        (Right myCategory) `shouldBe`
      Identity (Right (fullReq1, myCategory))
    it
      "Step2 Valid Logic Path For Edit Category : edit cur_path' = 4 to newPath = 2" $
      Category.checkLogicPathForEditCategory
        handleSpec
        5
        req2
        (Right myCategoryAfterStep1) `shouldBe`
      Identity (Right (fullReq2, myCategoryAfterStep1))
    it
      "Step3 Valid Logic Path For Edit Category : edit cur_path' = 6 to newPath = 1" $
      Category.checkLogicPathForEditCategory
        handleSpec
        11
        req3
        (Right myCategoryAfterStep2) `shouldBe`
      Identity (Right (fullReq3, myCategoryAfterStep2))
    it
      "Step4 Valid Logic Path For Edit Category : edit cur_path' = 7 to newPath = 3.1" $
      Category.checkLogicPathForEditCategory
        handleSpec
        12
        req4
        (Right myCategoryAfterStep3) `shouldBe`
      Identity (Right (fullReq4, myCategoryAfterStep3))
    it
      "Step5 Valid Logic Path For Edit Category : edit cur_path' = 3.3.1 to newPath = 1.1" $
      Category.checkLogicPathForEditCategory
        handleSpec
        8
        req5
        (Right myCategoryAfterStep4) `shouldBe`
      Identity (Right (fullReq5, myCategoryAfterStep4))
    it
      "Step6 Valid Logic Path For Edit Category : edit cur_path' = 7 to newPath = 3.3.1" $
      Category.checkLogicPathForEditCategory
        handleSpec
        13
        req6
        (Right myCategoryAfterStep5) `shouldBe`
      Identity (Right (fullReq6, myCategoryAfterStep5))
    it
      "Step6 Valid Logic Path For Edit Category : edit cur_path' = 3.3.2 to newPath = 3.3.1.1" $
      Category.checkLogicPathForEditCategory
        handleSpec
        9
        req7
        (Right myCategoryAfterStep6) `shouldBe`
      Identity (Right (fullReq7, myCategoryAfterStep6))
    it
      "Invalid Logic Path For Edit Category : Parent DOES NOT become a child of himself" $
      Category.checkLogicPathForEditCategory
        handleSpec
        5
        req11
        (Right myCategory) `shouldBe`
      Identity (Left error5)
    it "Invalid  Logic Path For Edit Category : Don`t do a hole in numbering" $
      Category.checkLogicPathForEditCategory
        handleSpec
        1
        req12
        (Right myCategory) `shouldBe`
      Identity (Left error6)

myCategory :: [DataTypes.Category]
myCategory =
  [ DataTypes.Category
      {categoryPath = "1", categoryId = 1, categoryName = "Ваграм Феликсович"}
  , DataTypes.Category
      {categoryPath = "2", categoryId = 2, categoryName = "Борис"}
  , DataTypes.Category
      {categoryPath = "3", categoryId = 3, categoryName = "Женя"}
  , DataTypes.Category
      {categoryPath = "4", categoryId = 4, categoryName = "Зоя"}
  , DataTypes.Category
      {categoryPath = "5", categoryId = 5, categoryName = "Ваграм"}
  , DataTypes.Category
      { categoryPath = "5.1"
      , categoryId = 6
      , categoryName = "Ваграм Александрович"
      }
  , DataTypes.Category
      {categoryPath = "5.2", categoryId = 7, categoryName = "Ваграм Олегович"}
  , DataTypes.Category
      {categoryPath = "5.2.1", categoryId = 8, categoryName = "Анна Борисовна"}
  , DataTypes.Category
      { categoryPath = "5.2.2"
      , categoryId = 9
      , categoryName = "Ваграм Олегович Саркисян - Иванов"
      }
  , DataTypes.Category
      {categoryPath = "6", categoryId = 10, categoryName = "Коля"}
  , DataTypes.Category
      {categoryPath = "7", categoryId = 11, categoryName = "Aнна"}
  , DataTypes.Category
      {categoryPath = "8", categoryId = 12, categoryName = "Ваграм Ааронович"}
  , DataTypes.Category
      { categoryPath = "9"
      , categoryId = 13
      , categoryName = "Ваграм Олегович Саркисян"
      }
  ]

-- Parent DOES NOT become a child of himself
req11 :: DataTypes.EditCategoryRequest
req11 =
  DataTypes.EditCategoryRequest {newPath = Just "5.2.3", newCategory = Nothing}

error5 :: ErrorTypes.AddEditCategoryError
error5 = ErrorTypes.InvalidValuePath $ ErrorTypes.InvalidContent []

-- Don`t do a hole in numbering
req12 :: DataTypes.EditCategoryRequest
req12 =
  DataTypes.EditCategoryRequest {newPath = Just "11", newCategory = Nothing}

error6 :: ErrorTypes.AddEditCategoryError
error6 = ErrorTypes.InvalidValuePath $ ErrorTypes.InvalidContent []

-- STEP1  Move Vagram Feliksovich to the second level after Vagram Olegovich
req1 :: DataTypes.EditCategoryRequest
req1 =
  DataTypes.EditCategoryRequest {newPath = Just "5.3", newCategory = Nothing}

fullReq1 :: CategoryHelpTypes.EditCategoryFullRequest
fullReq1 =
  CategoryHelpTypes.EditCategoryFullRequest
    { id' = 1
    , curPath' = "1"
    , curCategory' = "Ваграм Феликсович"
    , curLevel' = 1
    , newPath' = "5.3"
    , newCategory' = "Ваграм Феликсович"
    , newLevel' = 2
    , newPathStayAfterId' = 9
    }

-- STEP2 Put Vagram with all the children in Zhenya's place (Zhenya goes downstairs)
myCategoryAfterStep1 :: [DataTypes.Category]
myCategoryAfterStep1 =
  [ DataTypes.Category
      {categoryPath = "1", categoryId = 2, categoryName = "Борис"}
  , DataTypes.Category
      {categoryPath = "2", categoryId = 3, categoryName = "Женя"}
  , DataTypes.Category
      {categoryPath = "3", categoryId = 4, categoryName = "Зоя"}
  , DataTypes.Category
      {categoryPath = "4", categoryId = 5, categoryName = "Ваграм"}
  , DataTypes.Category
      { categoryPath = "4.1"
      , categoryId = 6
      , categoryName = "Ваграм Александрович"
      }
  , DataTypes.Category
      {categoryPath = "4.2", categoryId = 7, categoryName = "Ваграм Олегович"}
  , DataTypes.Category
      {categoryPath = "4.2.1", categoryId = 8, categoryName = "Анна Борисовна"}
  , DataTypes.Category
      { categoryPath = "4.2.2"
      , categoryId = 9
      , categoryName = "Ваграм Олегович Саркисян - Иванов"
      }
  , DataTypes.Category
      {categoryPath = "4.3", categoryId = 1, categoryName = "Ваграм Феликсович"}
  , DataTypes.Category
      {categoryPath = "5", categoryId = 10, categoryName = "Коля"}
  , DataTypes.Category
      {categoryPath = "6", categoryId = 11, categoryName = "Aнна"}
  , DataTypes.Category
      {categoryPath = "7", categoryId = 12, categoryName = "Ваграм Ааронович"}
  , DataTypes.Category
      { categoryPath = "8"
      , categoryId = 13
      , categoryName = "Ваграм Олегович Саркисян"
      }
  ]

req2 :: DataTypes.EditCategoryRequest
req2 = DataTypes.EditCategoryRequest {newPath = Just "2", newCategory = Nothing}

fullReq2 :: CategoryHelpTypes.EditCategoryFullRequest
fullReq2 =
  CategoryHelpTypes.EditCategoryFullRequest
    { id' = 5
    , curPath' = "4"
    , curCategory' = "Ваграм"
    , curLevel' = 1
    , newPath' = "2"
    , newCategory' = "Ваграм"
    , newLevel' = 1
    , newPathStayAfterId' = 2
    }

-- STEP3 Put Anna at the beginning
myCategoryAfterStep2 :: [DataTypes.Category]
myCategoryAfterStep2 =
  [ DataTypes.Category
      {categoryPath = "1", categoryId = 2, categoryName = "Борис"}
  , DataTypes.Category
      {categoryPath = "2", categoryId = 5, categoryName = "Ваграм"}
  , DataTypes.Category
      { categoryPath = "2.1"
      , categoryId = 6
      , categoryName = "Ваграм Александрович"
      }
  , DataTypes.Category
      {categoryPath = "2.2", categoryId = 7, categoryName = "Ваграм Олегович"}
  , DataTypes.Category
      {categoryPath = "2.2.1", categoryId = 8, categoryName = "Анна Борисовна"}
  , DataTypes.Category
      { categoryPath = "2.2.2"
      , categoryId = 9
      , categoryName = "Ваграм Олегович Саркисян - Иванов"
      }
  , DataTypes.Category
      {categoryPath = "2.3", categoryId = 1, categoryName = "Ваграм Феликсович"}
  , DataTypes.Category
      {categoryPath = "3", categoryId = 3, categoryName = "Женя"}
  , DataTypes.Category
      {categoryPath = "4", categoryId = 4, categoryName = "Зоя"}
  , DataTypes.Category
      {categoryPath = "5", categoryId = 10, categoryName = "Коля"}
  , DataTypes.Category
      {categoryPath = "6", categoryId = 11, categoryName = "Aнна"}
  , DataTypes.Category
      {categoryPath = "7", categoryId = 12, categoryName = "Ваграм Ааронович"}
  , DataTypes.Category
      { categoryPath = "8"
      , categoryId = 13
      , categoryName = "Ваграм Олегович Саркисян"
      }
  ]

req3 :: DataTypes.EditCategoryRequest
req3 = DataTypes.EditCategoryRequest {newPath = Just "1", newCategory = Nothing}

fullReq3 :: CategoryHelpTypes.EditCategoryFullRequest
fullReq3 =
  CategoryHelpTypes.EditCategoryFullRequest
    { id' = 11
    , curPath' = "6"
    , curCategory' = "Aнна"
    , curLevel' = 1
    , newPath' = "1"
    , newCategory' = "Aнна"
    , newLevel' = 1
    , newPathStayAfterId' = 0
    }

-- STEP4 Put Vagram Aaronovich before Vagram Alexandrovich
myCategoryAfterStep3 :: [DataTypes.Category]
myCategoryAfterStep3 =
  [ DataTypes.Category
      {categoryPath = "1", categoryId = 11, categoryName = "Aнна"}
  , DataTypes.Category
      {categoryPath = "2", categoryId = 2, categoryName = "Борис"}
  , DataTypes.Category
      {categoryPath = "3", categoryId = 5, categoryName = "Ваграм"}
  , DataTypes.Category
      { categoryPath = "3.1"
      , categoryId = 6
      , categoryName = "Ваграм Александрович"
      }
  , DataTypes.Category
      {categoryPath = "3.2", categoryId = 7, categoryName = "Ваграм Олегович"}
  , DataTypes.Category
      {categoryPath = "3.2.1", categoryId = 8, categoryName = "Анна Борисовна"}
  , DataTypes.Category
      { categoryPath = "3.2.2"
      , categoryId = 9
      , categoryName = "Ваграм Олегович Саркисян - Иванов"
      }
  , DataTypes.Category
      {categoryPath = "3.3", categoryId = 1, categoryName = "Ваграм Феликсович"}
  , DataTypes.Category
      {categoryPath = "4", categoryId = 3, categoryName = "Женя"}
  , DataTypes.Category
      {categoryPath = "5", categoryId = 4, categoryName = "Зоя"}
  , DataTypes.Category
      {categoryPath = "6", categoryId = 10, categoryName = "Коля"}
  , DataTypes.Category
      {categoryPath = "7", categoryId = 12, categoryName = "Ваграм Ааронович"}
  , DataTypes.Category
      { categoryPath = "8"
      , categoryId = 13
      , categoryName = "Ваграм Олегович Саркисян"
      }
  ]

req4 :: DataTypes.EditCategoryRequest
req4 =
  DataTypes.EditCategoryRequest {newPath = Just "3.1", newCategory = Nothing}

fullReq4 :: CategoryHelpTypes.EditCategoryFullRequest
fullReq4 =
  CategoryHelpTypes.EditCategoryFullRequest
    { id' = 12
    , curPath' = "7"
    , curCategory' = "Ваграм Ааронович"
    , curLevel' = 1
    , newPath' = "3.1"
    , newCategory' = "Ваграм Ааронович"
    , newLevel' = 2
    , newPathStayAfterId' = 5
    }

-- STEP5 Move Anna Borisovna to Anna's children
myCategoryAfterStep4 :: [DataTypes.Category]
myCategoryAfterStep4 =
  [ DataTypes.Category
      {categoryPath = "1", categoryId = 11, categoryName = "Aнна"}
  , DataTypes.Category
      {categoryPath = "2", categoryId = 2, categoryName = "Борис"}
  , DataTypes.Category
      {categoryPath = "3", categoryId = 5, categoryName = "Ваграм"}
  , DataTypes.Category
      {categoryPath = "3.1", categoryId = 12, categoryName = "Ваграм Ааронович"}
  , DataTypes.Category
      { categoryPath = "3.2"
      , categoryId = 6
      , categoryName = "Ваграм Александрович"
      }
  , DataTypes.Category
      {categoryPath = "3.3", categoryId = 7, categoryName = "Ваграм Олегович"}
  , DataTypes.Category
      {categoryPath = "3.3.1", categoryId = 8, categoryName = "Анна Борисовна"}
  , DataTypes.Category
      { categoryPath = "3.3.2"
      , categoryId = 9
      , categoryName = "Ваграм Олегович Саркисян - Иванов"
      }
  , DataTypes.Category
      {categoryPath = "3.4", categoryId = 1, categoryName = "Ваграм Феликсович"}
  , DataTypes.Category
      {categoryPath = "4", categoryId = 3, categoryName = "Женя"}
  , DataTypes.Category
      {categoryPath = "5", categoryId = 4, categoryName = "Зоя"}
  , DataTypes.Category
      {categoryPath = "6", categoryId = 10, categoryName = "Коля"}
  , DataTypes.Category
      { categoryPath = "7"
      , categoryId = 13
      , categoryName = "Ваграм Олегович Саркисян"
      }
  ]

req5 :: DataTypes.EditCategoryRequest
req5 =
  DataTypes.EditCategoryRequest {newPath = Just "1.1", newCategory = Nothing}

fullReq5 :: CategoryHelpTypes.EditCategoryFullRequest
fullReq5 =
  CategoryHelpTypes.EditCategoryFullRequest
    { id' = 8
    , curPath' = "3.3.1"
    , curCategory' = "Анна Борисовна"
    , curLevel' = 3
    , newPath' = "1.1"
    , newCategory' = "Анна Борисовна"
    , newLevel' = 2
    , newPathStayAfterId' = 11
    }

-- STEP6 Move Vahram Oleg Sargsyan one level down after Vahram Olegovich
myCategoryAfterStep5 :: [DataTypes.Category]
myCategoryAfterStep5 =
  [ DataTypes.Category
      {categoryPath = "1", categoryId = 11, categoryName = "Aнна"}
  , DataTypes.Category
      {categoryPath = "1.1", categoryId = 8, categoryName = "Анна Борисовна"}
  , DataTypes.Category
      {categoryPath = "2", categoryId = 2, categoryName = "Борис"}
  , DataTypes.Category
      {categoryPath = "3", categoryId = 5, categoryName = "Ваграм"}
  , DataTypes.Category
      {categoryPath = "3.1", categoryId = 12, categoryName = "Ваграм Ааронович"}
  , DataTypes.Category
      { categoryPath = "3.2"
      , categoryId = 6
      , categoryName = "Ваграм Александрович"
      }
  , DataTypes.Category
      {categoryPath = "3.3", categoryId = 7, categoryName = "Ваграм Олегович"}
  , DataTypes.Category
      { categoryPath = "3.3.1"
      , categoryId = 9
      , categoryName = "Ваграм Олегович Саркисян - Иванов"
      }
  , DataTypes.Category
      {categoryPath = "3.4", categoryId = 1, categoryName = "Ваграм Феликсович"}
  , DataTypes.Category
      {categoryPath = "4", categoryId = 3, categoryName = "Женя"}
  , DataTypes.Category
      {categoryPath = "5", categoryId = 4, categoryName = "Зоя"}
  , DataTypes.Category
      {categoryPath = "6", categoryId = 10, categoryName = "Коля"}
  , DataTypes.Category
      { categoryPath = "7"
      , categoryId = 13
      , categoryName = "Ваграм Олегович Саркисян"
      }
  ]

req6 :: DataTypes.EditCategoryRequest
req6 =
  DataTypes.EditCategoryRequest {newPath = Just "3.3.1", newCategory = Nothing}

fullReq6 :: CategoryHelpTypes.EditCategoryFullRequest
fullReq6 =
  CategoryHelpTypes.EditCategoryFullRequest
    { id' = 13
    , curPath' = "7"
    , curCategory' = "Ваграм Олегович Саркисян"
    , curLevel' = 1
    , newPath' = "3.3.1"
    , newCategory' = "Ваграм Олегович Саркисян"
    , newLevel' = 3
    , newPathStayAfterId' = 7
    }

-- STEP7 Vahram Olegovich Sargsyan - Ivanov move one level deeper
myCategoryAfterStep6 :: [DataTypes.Category]
myCategoryAfterStep6 =
  [ DataTypes.Category
      {categoryPath = "1", categoryId = 11, categoryName = "Aнна"}
  , DataTypes.Category
      {categoryPath = "1.1", categoryId = 8, categoryName = "Анна Борисовна"}
  , DataTypes.Category
      {categoryPath = "2", categoryId = 2, categoryName = "Борис"}
  , DataTypes.Category
      {categoryPath = "3", categoryId = 5, categoryName = "Ваграм"}
  , DataTypes.Category
      {categoryPath = "3.1", categoryId = 12, categoryName = "Ваграм Ааронович"}
  , DataTypes.Category
      { categoryPath = "3.2"
      , categoryId = 6
      , categoryName = "Ваграм Александрович"
      }
  , DataTypes.Category
      {categoryPath = "3.3", categoryId = 7, categoryName = "Ваграм Олегович"}
  , DataTypes.Category
      { categoryPath = "3.3.1"
      , categoryId = 13
      , categoryName = "Ваграм Олегович Саркисян"
      }
  , DataTypes.Category
      { categoryPath = "3.3.2"
      , categoryId = 9
      , categoryName = "Ваграм Олегович Саркисян - Иванов"
      }
  , DataTypes.Category
      {categoryPath = "3.4", categoryId = 1, categoryName = "Ваграм Феликсович"}
  , DataTypes.Category
      {categoryPath = "4", categoryId = 3, categoryName = "Женя"}
  , DataTypes.Category
      {categoryPath = "5", categoryId = 4, categoryName = "Зоя"}
  , DataTypes.Category
      {categoryPath = "6", categoryId = 10, categoryName = "Коля"}
  ]

req7 :: DataTypes.EditCategoryRequest
req7 =
  DataTypes.EditCategoryRequest
    {newPath = Just "3.3.1.1", newCategory = Nothing}

fullReq7 :: CategoryHelpTypes.EditCategoryFullRequest
fullReq7 =
  CategoryHelpTypes.EditCategoryFullRequest
    { id' = 9
    , curPath' = "3.3.2"
    , curCategory' = "Ваграм Олегович Саркисян - Иванов"
    , curLevel' = 3
    , newPath' = "3.3.1.1"
    , newCategory' = "Ваграм Олегович Саркисян - Иванов"
    , newLevel' = 4
    , newPathStayAfterId' = 13
    }
