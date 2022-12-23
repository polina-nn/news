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

module EndPoints.Lib.News.NewsIO
  ( addImageNewsIO -- use in EndPoints.AddOneNews, EndPoints.EditOneNews
  , toNews --use in EndPoints.GetNewsList,  EndPoints.GetAuthorsNewsList
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as SQL
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified EndPoints.Lib.Category.Category as Category
import qualified EndPoints.Lib.Lib as Lib
import qualified EndPoints.Lib.News.NewsHelpTypes as NewsHelpTypes
import qualified Logger
import qualified News
import qualified Types.DataTypes as DataTypes
import qualified Types.ErrorTypes as ErrorTypes

type IdImage = Int

type CategoryPath = String

-- | addImageNewsIO adding one pictures to the table of pictures when working with news
addImageNewsIO ::
     SQL.Connection
  -> News.Handle IO
  -> DataTypes.CreateImageRequest
  -> IO (Either ErrorTypes.AddEditNewsError IdImage)
addImageNewsIO conn h DataTypes.CreateImageRequest {..} = do
  content <- B.readFile image
  let imageDecodeBase64ByteString = Base64.decodeBase64Lenient content
  resId <-
    SQL.query_ conn [sql| select NEXTVAL('image_id_seq');|] :: IO [SQL.Only Int]
  case resId of
    [val] -> do
      let idIm = SQL.fromOnly val
      res <-
        SQL.execute
          conn
          "INSERT INTO image (image_id, image_name, image_type, image_content) VALUES (?,?,?,?)"
          (idIm, file, format, show imageDecodeBase64ByteString)
      case res of
        1 -> do
          Logger.logDebug (News.hLogHandle h) $
            T.pack ("addImageIO: OK! Image_id  " ++ show idIm)
          return $ Right idIm
        _ -> do
          Logger.logError (News.hLogHandle h) $
            T.pack $
            show $
            ErrorTypes.AddEditNewsSQLRequestError $
            ErrorTypes.SQLRequestError "addImageNewsIO: BAD!"
          return $
            Left $
            ErrorTypes.AddEditNewsSQLRequestError $
            ErrorTypes.SQLRequestError []
    _ -> do
      Logger.logError (News.hLogHandle h) $
        T.pack $
        show $
        ErrorTypes.AddEditNewsSQLRequestError $
        ErrorTypes.SQLRequestError "addImageNewsIO: BAD!"
      return $
        Left $
        ErrorTypes.AddEditNewsSQLRequestError $ ErrorTypes.SQLRequestError []

toNews ::
     SQL.Connection
  -> News.Handle IO
  -> NewsHelpTypes.DbNews
  -> IO (Either ErrorTypes.GetNewsError DataTypes.News)
toNews con h NewsHelpTypes.DbNews {..} = do
  categories <- getCategoriesIO con h dbNewsCategoryPath
  case categories of
    Left err -> return $ Left err
    Right cats -> do
      let news =
            DataTypes.News
              { newsTitle = dbNewsTitle
              , newsCreated = dbNewsCreated
              , newsAuthor = dbNewsAuthor
              , newsCategory = cats
              , newsText = dbNewsText
              , newsImages = imageUris dbNewsImagesId
              , newsPublished = dbNewsPublished
              }
      return $ Right news
  where
    imageUris :: [Int] -> [DataTypes.URI]
    imageUris [] = []
    imageUris xs = map (Lib.imageIdToURI h) xs

getCategoriesIO ::
     SQL.Connection
  -> News.Handle IO
  -> CategoryPath
  -> IO (Either ErrorTypes.GetNewsError [DataTypes.Category])
getCategoriesIO con h''' path = do
  res <-
    SQL.query
      con
      [sql| SELECT category_path, category_id, category_name FROM category WHERE ? LIKE category_path||'%' ORDER BY category_path;|]
      (SQL.Only path)
  case res of
    [] -> do
      Logger.logError (News.hLogHandle h''') $
        T.pack $
        show $
        ErrorTypes.GetNewsSQLRequestError $
        ErrorTypes.SQLRequestError "getNewsCategoryIO: getCategoriesIO : BAD "
      return $
        Left $ ErrorTypes.GetNewsSQLRequestError $ ErrorTypes.SQLRequestError []
    _ -> do
      let categories = Prelude.map Category.toCategories res
      Logger.logDebug (News.hLogHandle h''') $
        T.concat [T.pack "getCategoriesIO: OK! "]
      return $ Right categories
