{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module CheckLogicPathForEditCategSpec
  ( spec,
  )
where

import Control.Monad.Identity (Identity (Identity))
import qualified EndPoints.Lib.Category as Category
import qualified EndPoints.Lib.CategoryHelpTypes as CategoryHelpTypes
import Handle (handleSpec)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck ()
import qualified Types.DataTypes as DataTypes
import qualified Types.ErrorTypes as ErrorTypes

spec :: Spec
spec = describe "Check Logic Path For Edit Categ - if OK! Return Right (EditCategoryFullRequest, [DataTypes.Category]), else Left error  " $ do
  it "Step1 Valid Logic Path For Edit Category : edit cur_path' = 1 to new_path= 3" $
    Category.checkLogicPathForEditCateg handleSpec 1 req1 (Right myCategory) `shouldBe` Identity (Right (fullReq1, myCategory))
  it "Step2 Valid Logic Path For Edit Category : edit cur_path' = 4 to new_path = 2" $
    Category.checkLogicPathForEditCateg handleSpec 5 req2 (Right myCategoryAfterStep1) `shouldBe` Identity (Right (fullReq2, myCategoryAfterStep1))
  it "Step3 Valid Logic Path For Edit Category : edit cur_path' = 6 to new_path = 1" $
    Category.checkLogicPathForEditCateg handleSpec 11 req3 (Right myCategoryAfterStep2) `shouldBe` Identity (Right (fullReq3, myCategoryAfterStep2))
  it "Step4 Valid Logic Path For Edit Category : edit cur_path' = 7 to new_path = 3.1" $
    Category.checkLogicPathForEditCateg handleSpec 12 req4 (Right myCategoryAfterStep3) `shouldBe` Identity (Right (fullReq4, myCategoryAfterStep3))

  it "Step5 Valid Logic Path For Edit Category : edit cur_path' = 3.3.1 to new_path = 1.1" $
    Category.checkLogicPathForEditCateg handleSpec 8 req5 (Right myCategoryAfterStep4) `shouldBe` Identity (Right (fullReq5, myCategoryAfterStep4))
  it "Step6 Valid Logic Path For Edit Category : edit cur_path' = 7 to new_path = 3.3.1" $
    Category.checkLogicPathForEditCateg handleSpec 13 req6 (Right myCategoryAfterStep5) `shouldBe` Identity (Right (fullReq6, myCategoryAfterStep5))

  it "Step6 Valid Logic Path For Edit Category : edit cur_path' = 3.3.2 to new_path = 3.3.1.1" $
    Category.checkLogicPathForEditCateg handleSpec 9 req7 (Right myCategoryAfterStep6) `shouldBe` Identity (Right (fullReq7, myCategoryAfterStep6))

  it "Invalid Logic Path For Edit Category : Parent DOES NOT become a child of himself" $
    Category.checkLogicPathForEditCateg handleSpec 5 req11 (Right myCategory) `shouldBe` Identity (Left error5)
  it "Invalid  Logic Path For Edit Category : Don`t do a hole in numbering" $
    Category.checkLogicPathForEditCateg handleSpec 1 req12 (Right myCategory) `shouldBe` Identity (Left error6)

-- Будем наводить порядок в данной базе Step1 --
myCategory :: [DataTypes.Category]
myCategory =
  [ DataTypes.Category {category_path = "1", category_id = 1, category_name = "Ваграм Феликсович"},
    DataTypes.Category {category_path = "2", category_id = 2, category_name = "Борис"},
    DataTypes.Category {category_path = "3", category_id = 3, category_name = "Женя"},
    DataTypes.Category {category_path = "4", category_id = 4, category_name = "Зоя"},
    DataTypes.Category {category_path = "5", category_id = 5, category_name = "Ваграм"},
    DataTypes.Category {category_path = "5.1", category_id = 6, category_name = "Ваграм Александрович"},
    DataTypes.Category {category_path = "5.2", category_id = 7, category_name = "Ваграм Олегович"},
    DataTypes.Category {category_path = "5.2.1", category_id = 8, category_name = "Анна Борисовна"},
    DataTypes.Category {category_path = "5.2.2", category_id = 9, category_name = "Ваграм Олегович Саркисян - Иванов"},
    DataTypes.Category {category_path = "6", category_id = 10, category_name = "Коля"},
    DataTypes.Category {category_path = "7", category_id = 11, category_name = "Aнна"},
    DataTypes.Category {category_path = "8", category_id = 12, category_name = "Ваграм Ааронович"},
    DataTypes.Category {category_path = "9", category_id = 13, category_name = "Ваграм Олегович Саркисян"}
  ]

-- Parent DOES NOT become a child of himself
req11 :: DataTypes.EditCategoryRequest
req11 =
  DataTypes.EditCategoryRequest
    { new_path = Just "5.2.3",
      new_category = Nothing -- имя не меняю
    }

error5 :: ErrorTypes.AddEditCategoryError
error5 =
  ErrorTypes.InvalidValuePath $ ErrorTypes.InvalidContent []

-- Don`t do a hole in numbering
req12 :: DataTypes.EditCategoryRequest
req12 =
  DataTypes.EditCategoryRequest
    { new_path = Just "11",
      new_category = Nothing -- имя не меняю
    }

error6 :: ErrorTypes.AddEditCategoryError
error6 = ErrorTypes.InvalidValuePath $ ErrorTypes.InvalidContent []

-- STEP1  Move Vagram Feliksovich to the second level after Vagram Olegovich
req1 :: DataTypes.EditCategoryRequest
req1 =
  DataTypes.EditCategoryRequest
    { new_path = Just "5.3",
      new_category = Nothing
    }

fullReq1 :: CategoryHelpTypes.EditCategoryFullRequest
fullReq1 =
  CategoryHelpTypes.EditCategoryFullRequest
    { id' = 1,
      cur_path' = "1",
      cur_category' = "Ваграм Феликсович",
      cur_level' = 1,
      new_path' = "5.3",
      new_category' = "Ваграм Феликсович",
      new_level' = 2,
      new_path_stay_after_id' = 9
    }

-- STEP2 Put Vagram with all the children in Zhenya's place (Zhenya goes downstairs)
myCategoryAfterStep1 :: [DataTypes.Category]
myCategoryAfterStep1 =
  [ DataTypes.Category {category_path = "1", category_id = 2, category_name = "Борис"},
    DataTypes.Category {category_path = "2", category_id = 3, category_name = "Женя"},
    DataTypes.Category {category_path = "3", category_id = 4, category_name = "Зоя"},
    DataTypes.Category {category_path = "4", category_id = 5, category_name = "Ваграм"},
    DataTypes.Category {category_path = "4.1", category_id = 6, category_name = "Ваграм Александрович"},
    DataTypes.Category {category_path = "4.2", category_id = 7, category_name = "Ваграм Олегович"},
    DataTypes.Category {category_path = "4.2.1", category_id = 8, category_name = "Анна Борисовна"},
    DataTypes.Category {category_path = "4.2.2", category_id = 9, category_name = "Ваграм Олегович Саркисян - Иванов"},
    DataTypes.Category {category_path = "4.3", category_id = 1, category_name = "Ваграм Феликсович"},
    DataTypes.Category {category_path = "5", category_id = 10, category_name = "Коля"},
    DataTypes.Category {category_path = "6", category_id = 11, category_name = "Aнна"},
    DataTypes.Category {category_path = "7", category_id = 12, category_name = "Ваграм Ааронович"},
    DataTypes.Category {category_path = "8", category_id = 13, category_name = "Ваграм Олегович Саркисян"}
  ]

req2 :: DataTypes.EditCategoryRequest
req2 =
  DataTypes.EditCategoryRequest
    { new_path = Just "2",
      new_category = Nothing -- имя не меняю
    }

fullReq2 :: CategoryHelpTypes.EditCategoryFullRequest
fullReq2 =
  CategoryHelpTypes.EditCategoryFullRequest
    { id' = 5,
      cur_path' = "4",
      cur_category' = "Ваграм",
      cur_level' = 1,
      new_path' = "2",
      new_category' = "Ваграм",
      new_level' = 1,
      new_path_stay_after_id' = 2
    }

-- STEP3 Put Anna at the beginning
myCategoryAfterStep2 :: [DataTypes.Category]
myCategoryAfterStep2 =
  [ DataTypes.Category {category_path = "1", category_id = 2, category_name = "Борис"},
    DataTypes.Category {category_path = "2", category_id = 5, category_name = "Ваграм"},
    DataTypes.Category {category_path = "2.1", category_id = 6, category_name = "Ваграм Александрович"},
    DataTypes.Category {category_path = "2.2", category_id = 7, category_name = "Ваграм Олегович"},
    DataTypes.Category {category_path = "2.2.1", category_id = 8, category_name = "Анна Борисовна"},
    DataTypes.Category {category_path = "2.2.2", category_id = 9, category_name = "Ваграм Олегович Саркисян - Иванов"},
    DataTypes.Category {category_path = "2.3", category_id = 1, category_name = "Ваграм Феликсович"},
    DataTypes.Category {category_path = "3", category_id = 3, category_name = "Женя"},
    DataTypes.Category {category_path = "4", category_id = 4, category_name = "Зоя"},
    DataTypes.Category {category_path = "5", category_id = 10, category_name = "Коля"},
    DataTypes.Category {category_path = "6", category_id = 11, category_name = "Aнна"},
    DataTypes.Category {category_path = "7", category_id = 12, category_name = "Ваграм Ааронович"},
    DataTypes.Category {category_path = "8", category_id = 13, category_name = "Ваграм Олегович Саркисян"}
  ]

req3 :: DataTypes.EditCategoryRequest
req3 =
  DataTypes.EditCategoryRequest
    { new_path = Just "1",
      new_category = Nothing -- имя не меняю
    }

fullReq3 :: CategoryHelpTypes.EditCategoryFullRequest
fullReq3 =
  CategoryHelpTypes.EditCategoryFullRequest
    { id' = 11,
      cur_path' = "6",
      cur_category' = "Aнна",
      cur_level' = 1,
      new_path' = "1",
      new_category' = "Aнна",
      new_level' = 1,
      new_path_stay_after_id' = 0
    }

-- STEP4 Put Vagram Aaronovich before Vagram Alexandrovich
myCategoryAfterStep3 :: [DataTypes.Category]
myCategoryAfterStep3 =
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

req4 :: DataTypes.EditCategoryRequest
req4 =
  DataTypes.EditCategoryRequest
    { new_path = Just "3.1",
      new_category = Nothing -- имя не меняю
    }

fullReq4 :: CategoryHelpTypes.EditCategoryFullRequest
fullReq4 =
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

-- STEP5 Move Anna Borisovna to Anna's children
myCategoryAfterStep4 :: [DataTypes.Category]
myCategoryAfterStep4 =
  [ DataTypes.Category {category_path = "1", category_id = 11, category_name = "Aнна"},
    DataTypes.Category {category_path = "2", category_id = 2, category_name = "Борис"},
    DataTypes.Category {category_path = "3", category_id = 5, category_name = "Ваграм"},
    DataTypes.Category {category_path = "3.1", category_id = 12, category_name = "Ваграм Ааронович"},
    DataTypes.Category {category_path = "3.2", category_id = 6, category_name = "Ваграм Александрович"},
    DataTypes.Category {category_path = "3.3", category_id = 7, category_name = "Ваграм Олегович"},
    DataTypes.Category {category_path = "3.3.1", category_id = 8, category_name = "Анна Борисовна"},
    DataTypes.Category {category_path = "3.3.2", category_id = 9, category_name = "Ваграм Олегович Саркисян - Иванов"},
    DataTypes.Category {category_path = "3.4", category_id = 1, category_name = "Ваграм Феликсович"},
    DataTypes.Category {category_path = "4", category_id = 3, category_name = "Женя"},
    DataTypes.Category {category_path = "5", category_id = 4, category_name = "Зоя"},
    DataTypes.Category {category_path = "6", category_id = 10, category_name = "Коля"},
    DataTypes.Category {category_path = "7", category_id = 13, category_name = "Ваграм Олегович Саркисян"}
  ]

req5 :: DataTypes.EditCategoryRequest
req5 =
  DataTypes.EditCategoryRequest
    { new_path = Just "1.1",
      new_category = Nothing
    }

fullReq5 :: CategoryHelpTypes.EditCategoryFullRequest
fullReq5 =
  CategoryHelpTypes.EditCategoryFullRequest
    { id' = 8,
      cur_path' = "3.3.1",
      cur_category' = "Анна Борисовна",
      cur_level' = 3,
      new_path' = "1.1",
      new_category' = "Анна Борисовна",
      new_level' = 2,
      new_path_stay_after_id' = 11
    }

-- STEP6 Move Vahram Oleg Sargsyan one level down after Vahram Olegovich
myCategoryAfterStep5 :: [DataTypes.Category]
myCategoryAfterStep5 =
  [ DataTypes.Category {category_path = "1", category_id = 11, category_name = "Aнна"},
    DataTypes.Category {category_path = "1.1", category_id = 8, category_name = "Анна Борисовна"},
    DataTypes.Category {category_path = "2", category_id = 2, category_name = "Борис"},
    DataTypes.Category {category_path = "3", category_id = 5, category_name = "Ваграм"},
    DataTypes.Category {category_path = "3.1", category_id = 12, category_name = "Ваграм Ааронович"},
    DataTypes.Category {category_path = "3.2", category_id = 6, category_name = "Ваграм Александрович"},
    DataTypes.Category {category_path = "3.3", category_id = 7, category_name = "Ваграм Олегович"},
    DataTypes.Category {category_path = "3.3.1", category_id = 9, category_name = "Ваграм Олегович Саркисян - Иванов"},
    DataTypes.Category {category_path = "3.4", category_id = 1, category_name = "Ваграм Феликсович"},
    DataTypes.Category {category_path = "4", category_id = 3, category_name = "Женя"},
    DataTypes.Category {category_path = "5", category_id = 4, category_name = "Зоя"},
    DataTypes.Category {category_path = "6", category_id = 10, category_name = "Коля"},
    DataTypes.Category {category_path = "7", category_id = 13, category_name = "Ваграм Олегович Саркисян"}
  ]

req6 :: DataTypes.EditCategoryRequest
req6 =
  DataTypes.EditCategoryRequest
    { new_path = Just "3.3.1",
      new_category = Nothing
    }

fullReq6 :: CategoryHelpTypes.EditCategoryFullRequest
fullReq6 =
  CategoryHelpTypes.EditCategoryFullRequest
    { id' = 13,
      cur_path' = "7",
      cur_category' = "Ваграм Олегович Саркисян",
      cur_level' = 1,
      new_path' = "3.3.1",
      new_category' = "Ваграм Олегович Саркисян",
      new_level' = 3,
      new_path_stay_after_id' = 7
    }

-- STEP7 Vahram Olegovich Sargsyan - Ivanov move one level deeper
myCategoryAfterStep6 :: [DataTypes.Category]
myCategoryAfterStep6 =
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

req7 :: DataTypes.EditCategoryRequest
req7 =
  DataTypes.EditCategoryRequest
    { new_path = Just "3.3.1.1",
      new_category = Nothing
    }

fullReq7 :: CategoryHelpTypes.EditCategoryFullRequest
fullReq7 =
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
