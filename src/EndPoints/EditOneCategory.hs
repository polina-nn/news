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

-- for import Database.PostgreSQL.Simple.SqlQQ (sql)
module EndPoints.EditOneCategory
  ( editOneCategory
  , editCategory
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

editOneCategory ::
     News.Handle IO
  -> DataTypes.Db
  -> DataTypes.User
  -> Int
  -> DataTypes.EditCategoryRequest
  -> Handler DataTypes.Category
editOneCategory h DataTypes.Db {..} user catId r =
  (>>=)
    (liftIO $ _editCategory (h, user, catId, r))
    ToHttpResponse.toHttpResponse

editCategory ::
     SQL.Connection
  -> (News.Handle IO, DataTypes.User, Int, DataTypes.EditCategoryRequest)
  -> IO (Either ErrorTypes.AddEditCategoryError DataTypes.Category)
editCategory conn (h, user, catId, r) = do
  allCheck <-
    Lib.checkUserAdmin h user >>= ceckIdIO conn h catId r >>= checkSyntaxPath h >>=
    CategoryIO.getAllCategoriesIO conn h >>=
    Category.checkLogicPathForEditCateg h catId r
  case allCheck :: Either ErrorTypes.AddEditCategoryError ( CategoryHelpTypes.EditCategoryFullRequest
                                                          , [DataTypes.Category]) of
    Left err -> return $ Left err
    Right (editCategFullReq, categs) -> do
      Logger.logDebug (News.hLogHandle h) $
        T.concat
          [ T.pack "editCategory:allCheck: OK!  \n"
          , ToText.toText editCategFullReq
          ]
      Logger.logDebug (News.hLogHandle h) $
        T.concat (Prelude.map ToText.toText categs)
      case Category.changePathsForEditCateg editCategFullReq categs :: Maybe [CategoryHelpTypes.EditCategory] of
        Nothing -> do
          Logger.logError (News.hLogHandle h) $
            T.pack $
            show $
            ErrorTypes.InvalidValuePath $
            ErrorTypes.InvalidContent
              "check Developer Error, then update categorys table"
          return $
            Left $ ErrorTypes.InvalidValuePath $ ErrorTypes.InvalidContent []
        Just tochangePaths -> do
          rez <-
            catch
              (CategoryIO.changePathCategoriesIO conn h tochangePaths >>=
               editCategoryNameIO conn h editCategFullReq)
              handleError
          case rez of
            (Right newCategory) -> do
              Logger.logInfo (News.hLogHandle h) $
                T.concat
                  [T.pack "editCategory: OK!  \n", ToText.toText newCategory]
              return rez
            (Left newCategoryError) -> do
              Logger.logInfo
                (News.hLogHandle h)
                (T.pack ("editCategory: BAD! " ++ show newCategoryError))
              return rez
  where
    handleError ::
         SomeException
      -> IO (Either ErrorTypes.AddEditCategoryError DataTypes.Category)
    handleError (SomeException e) = do
      let errMsg = displayException e
      Logger.logError
        (News.hLogHandle h)
        (T.pack ("editCategory:handleError:" ++ show errMsg))
      throwIO e

-- | проверяю есть ли запись с данной редактируемой id категории в базе данных (то есть есть ли  id = 7 в базе http://localhost:8080/category/7 )
ceckIdIO ::
     SQL.Connection
  -> News.Handle IO
  -> Int
  -> DataTypes.EditCategoryRequest
  -> Either ErrorTypes.InvalidAdminPermission DataTypes.User
  -> IO (Either ErrorTypes.AddEditCategoryError DataTypes.EditCategoryRequest)
ceckIdIO _ _ _ _ (Left err) =
  return $ Left $ ErrorTypes.InvalidPermissionAddEditCategory err
ceckIdIO conn h catId r (Right _) = do
  res <-
    SQL.query
      conn
      [sql| SELECT EXISTS (SELECT category_id  FROM category WHERE category_id = ?) |]
      (SQL.Only catId) :: IO [SQL.Only Bool]
  case res of
    [] -> do
      Logger.logError (News.hLogHandle h) $
        T.pack $
        show $
        ErrorTypes.AddEditCategorySQLRequestError $
        ErrorTypes.SQLRequestError "ceckIdIO! Dont ceckId category"
      return $
        Left $
        ErrorTypes.AddEditCategorySQLRequestError $
        ErrorTypes.SQLRequestError []
    _ ->
      if SQL.fromOnly $ head res
        then (do Logger.logDebug (News.hLogHandle h) $
                   T.pack
                     ("ceckId: OK! Category whith id " ++
                      show catId ++ " exists")
                 return $ Right r)
        else (do Logger.logError (News.hLogHandle h) $
                   T.pack $
                   show $
                   ErrorTypes.InvalidCategoryId $
                   ErrorTypes.InvalidId
                     ("ceckId: BAD! Category whith id " ++
                      show catId ++ " not exists")
                 return $
                   Left $ ErrorTypes.InvalidCategoryId $ ErrorTypes.InvalidId [])

checkSyntaxPath ::
     News.Handle IO
  -> Either ErrorTypes.AddEditCategoryError DataTypes.EditCategoryRequest
  -> IO (Either ErrorTypes.AddEditCategoryError DataTypes.EditCategoryRequest)
checkSyntaxPath _ (Left er) = return $ Left er
checkSyntaxPath _ r@(Right DataTypes.EditCategoryRequest { DataTypes.new_path = Nothing
                                                         , DataTypes.new_category = _
                                                         }) = return r
checkSyntaxPath h r@(Right DataTypes.EditCategoryRequest { DataTypes.new_path = Just path
                                                         , DataTypes.new_category = _
                                                         }) =
  if Category.validSyntaxPath path
    then return r
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

editCategoryNameIO ::
     SQL.Connection
  -> News.Handle IO
  -> CategoryHelpTypes.EditCategoryFullRequest
  -> Either ErrorTypes.AddEditCategoryError Int
  -> IO (Either ErrorTypes.AddEditCategoryError DataTypes.Category)
editCategoryNameIO _ _ _ (Left er) = return $ Left er
editCategoryNameIO conn h CategoryHelpTypes.EditCategoryFullRequest {..} (Right _) = do
  res <-
    SQL.execute
      conn
      [sql| UPDATE category SET category_name = ? WHERE category_id = ? ;|]
      (new_category', id')
  case read (show res) :: Int of
    1 -> do
      rez_new_path <-
        SQL.query
          conn
          [sql| SELECT category_path FROM category WHERE category_id = ? |]
          (SQL.Only id')
      let rez_new_path' = SQL.fromOnly . head $ rez_new_path :: String
      let editedCategory =
            Category.toCategories (rez_new_path', id', new_category')
      Logger.logInfo (News.hLogHandle h) $
        T.concat
          [ T.pack "editCategoryIO: OK! UPDATE  category: "
          , ToText.toText editedCategory
          ]
      return $ Right editedCategory
    _ -> do
      Logger.logError (News.hLogHandle h) $
        T.pack $
        show $
        ErrorTypes.AddEditCategorySQLRequestError $
        ErrorTypes.SQLRequestError "editCategoryIO: BAD! Dont UPDATE  category"
      return $
        Left $
        ErrorTypes.AddEditCategorySQLRequestError $
        ErrorTypes.SQLRequestError []
