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

module EndPoints.AddOneCategory
  ( addOneCategory
  , addCategory
  ) where

import Control.Exception.Base
  ( Exception(displayException)
  , SomeException(SomeException)
  , catch
  , throwIO
  )
import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as SQL
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified EndPoints.Lib.Category.Category as Category
import qualified EndPoints.Lib.Category.CategoryHelpTypes as CategoryHelpTypes
import qualified EndPoints.Lib.Category.CategoryIO as CategoryIO
import qualified EndPoints.Lib.Lib as Lib
import qualified EndPoints.Lib.ToHttpResponse as ToHttpResponse
import qualified EndPoints.Lib.ToText as ToText
import qualified Logger
import qualified News
import Servant (Handler)
import qualified Types.DataTypes as DataTypes
import qualified Types.ErrorTypes as ErrorTypes

addOneCategory ::
     News.Handle IO
  -> DataTypes.Db
  -> DataTypes.User
  -> DataTypes.CreateCategoryRequest
  -> Handler DataTypes.Category
addOneCategory h DataTypes.Db {..} user createCategoryReq =
  (>>=)
    (liftIO $ _addCategory (h, user, createCategoryReq))
    ToHttpResponse.toHttpResponse

addCategory ::
     SQL.Connection
  -> (News.Handle IO, DataTypes.User, DataTypes.CreateCategoryRequest)
  -> IO (Either ErrorTypes.AddEditCategoryError DataTypes.Category)
addCategory conn (h, user, r) = do
  allCheck <-
    Lib.checkUserAdmin h user >>= checkSyntaxPath h r >>=
    CategoryIO.getAllCategoriesIO conn h >>=
    Category.checkLogicPathForAddCategory h r
  case allCheck :: Either ErrorTypes.AddEditCategoryError ( DataTypes.CreateCategoryRequest
                                                          , [DataTypes.Category]) of
    Left err -> return $ Left err
    Right (req, categories) -> do
      let toChangePaths =
            Category.changePathForAddCategory req categories :: [CategoryHelpTypes.EditCategory]
      rez <-
        catch
          (CategoryIO.changePathCategoriesIO conn h toChangePaths >>=
           addCategoryIO conn h req)
          handleError
      case rez of
        (Right newCategory) -> do
          Logger.logInfo (News.hLogHandle h) $
            T.concat [T.pack "addCategory: OK! \n", ToText.toText newCategory]
          return rez
        (Left newCategoryError) -> do
          Logger.logInfo
            (News.hLogHandle h)
            (T.pack ("addCategory: BAD! " ++ show newCategoryError))
          return rez
  where
    handleError ::
         SomeException
      -> IO (Either ErrorTypes.AddEditCategoryError DataTypes.Category)
    handleError (SomeException e) = do
      let errMsg = displayException e
      Logger.logError
        (News.hLogHandle h)
        (T.pack ("addCategory:handleError:" ++ show errMsg))
      throwIO e

checkSyntaxPath ::
     News.Handle IO
  -> DataTypes.CreateCategoryRequest
  -> Either ErrorTypes.InvalidAdminPermission DataTypes.User
  -> IO (Either ErrorTypes.AddEditCategoryError DataTypes.CreateCategoryRequest)
checkSyntaxPath _ _ (Left err) =
  return $ Left $ ErrorTypes.InvalidPermissionAddEditCategory err
checkSyntaxPath h r@DataTypes.CreateCategoryRequest {..} (Right _) =
  if Category.validSyntaxPath path
    then return $ Right r
    else do
      Logger.logError (News.hLogHandle h) $
        T.pack $
        show $
        ErrorTypes.InvalidSyntaxPath $
        ErrorTypes.InvalidContent
          ("checkSyntaxPath: BAD! Path is not valid! Only digits(not zero begin) and points must have! " ++
           path)
      return $
        Left $ ErrorTypes.InvalidSyntaxPath $ ErrorTypes.InvalidContent []

addCategoryIO ::
     SQL.Connection
  -> News.Handle IO
  -> DataTypes.CreateCategoryRequest
  -> Either ErrorTypes.AddEditCategoryError Int
  -> IO (Either ErrorTypes.AddEditCategoryError DataTypes.Category)
addCategoryIO _ _ _ (Left er) = return $ Left er
addCategoryIO conn h DataTypes.CreateCategoryRequest {..} (Right _) = do
  res <-
    SQL.execute
      conn
      [sql| INSERT INTO  category (category_path, category_name)  VALUES (?,?) ;|]
      (path, category)
  Logger.logInfo
    (News.hLogHandle h)
    (T.pack ("addCategoryIO: OK! INSERT INTO " ++ "\n" ++ show res))
  case read (show res) :: Int of
    1 -> do
      resId <-
        SQL.query
          conn
          [sql| SELECT category_id  FROM category WHERE category_path = ?; |]
          (SQL.Only path) :: IO [SQL.Only Int]
      case resId of
        [val] -> do
          let rez = SQL.fromOnly val
          return $
            Right
              (DataTypes.Category
                 { categoryPath = path
                 , categoryId = rez
                 , categoryName = category
                 })
        _ -> do
          Logger.logError (News.hLogHandle h) $
            T.pack $
            show $
            ErrorTypes.AddEditCategorySQLRequestError $
            ErrorTypes.SQLRequestError
              "addCategoryIO! Don't get Id category or category_path is not unique "
          return $
            Left $
            ErrorTypes.AddEditCategorySQLRequestError $
            ErrorTypes.SQLRequestError []
    _ -> do
      Logger.logError (News.hLogHandle h) $
        T.pack $
        show $
        ErrorTypes.AddEditCategorySQLRequestError $
        ErrorTypes.SQLRequestError
          ("addCategoryIO! Don't INSERT INTO  category" ++ path)
      return $
        Left $
        ErrorTypes.AddEditCategorySQLRequestError $
        ErrorTypes.SQLRequestError []
