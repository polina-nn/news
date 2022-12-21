{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module EndPoints.Lib.Category.CategoryIO
  ( changePathCategoriesIO
  , getAllCategoriesIO
  ) where

import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as SQL
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified EndPoints.Lib.Category.Category as Category
import qualified EndPoints.Lib.Category.CategoryHelpTypes as CategoryHelpTypes
import qualified Logger
import qualified News
import qualified Types.DataTypes as DataTypes
import qualified Types.ErrorTypes as ErrorTypes

changePathCategoriesIO ::
     SQL.Connection
  -> News.Handle IO
  -> [CategoryHelpTypes.EditCategory]
  -> IO (Either ErrorTypes.AddEditCategoryError Int)
changePathCategoriesIO _ h [] = do
  Logger.logInfo
    (News.hLogHandle h)
    "changePathCategoriesIO: OK! Request don't change path"
  return $ Right 0
changePathCategoriesIO conn h rs = count
  where
    count :: IO (Either ErrorTypes.AddEditCategoryError Int)
    count = do
      rez <- mapM (changePathOneCategoryIO conn h) rs
      if length (filter (\x -> x == Right 1) rez) == length rez
        then return $ Right $ length rez
        else do
          Logger.logError (News.hLogHandle h) $
            T.pack $
            show $
            ErrorTypes.AddEditCategorySQLRequestError $
            ErrorTypes.SQLRequestError
              ("changePathOneCategoryIO:BAD! Don't UPDATE  all categories. Update only" ++
               show (length rez))
          return $
            Left $
            ErrorTypes.AddEditCategorySQLRequestError $
            ErrorTypes.SQLRequestError []

changePathOneCategoryIO ::
     SQL.Connection
  -> News.Handle IO
  -> CategoryHelpTypes.EditCategory
  -> IO (Either ErrorTypes.AddEditCategoryError Int)
changePathOneCategoryIO conn h CategoryHelpTypes.EditCategory {..} = do
  res <-
    SQL.execute
      conn
      [sql| UPDATE  category SET category_path = ? WHERE category_id = ? ;|]
      (newPath, _id)
  case read (show res) :: Int of
    1 -> do
      Logger.logInfo
        (News.hLogHandle h)
        (T.pack
           ("changePathOneCategoryIO: OK!  " ++
            "\n" ++ "id =  " ++ show _id ++ " path = " ++ newPath))
      return $ Right 1
    _ -> do
      Logger.logError (News.hLogHandle h) $
        T.pack $
        show $
        ErrorTypes.AddEditCategorySQLRequestError
          (ErrorTypes.SQLRequestError
             "changePathOneCategoryIO:BAD! Don't UPDATE  category")
      return $
        Left $
        ErrorTypes.AddEditCategorySQLRequestError $
        ErrorTypes.SQLRequestError []

-- | getAllCategories --
-- when we add the first category to the response, we get an empty array in getAllCategoriesIO
getAllCategoriesIO ::
     Show m
  => SQL.Connection
  -> News.Handle IO
  -> Either ErrorTypes.AddEditCategoryError m
  -> IO (Either ErrorTypes.AddEditCategoryError [DataTypes.Category])
getAllCategoriesIO _ _ (Left err) = return $ Left err
getAllCategoriesIO conn _ (Right _) = do
  res <-
    SQL.query_
      conn
      [sql| SELECT category_path, category_id, category_name
                FROM category 
                ORDER BY category_path |]
  case res of
    [] -> return $ Right []
    _ -> return $ Right $ Prelude.map Category.toCategories res
