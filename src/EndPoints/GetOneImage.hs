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

module EndPoints.GetOneImage
  ( getOneImage
  , oneImage
  ) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as SQL
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified EndPoints.Lib.ToHttpResponse as ToHttpResponse
import qualified Logger
import qualified News
import Servant (Handler)
import qualified Types.DataTypes as DataTypes
import qualified Types.ErrorTypes as ErrorTypes

getOneImage :: News.Handle IO -> DataTypes.Db -> Integer -> Handler B.ByteString
getOneImage h DataTypes.Db {..} idImage =
  (>>=) (liftIO $ _oneImage (h, idImage)) ToHttpResponse.toHttpResponse

oneImage ::
     SQL.Connection
  -> (News.Handle IO, Integer)
  -> IO (Either ErrorTypes.GetImageError B.ByteString)
oneImage conn (h, id') = do
  Logger.logDebug (News.hLogHandle h) $ T.pack "Request: Get One Image"
  resCeckId <- ceckId conn (h, id')
  case resCeckId of
    Right _ -> do
      res <-
        SQL.query
          conn
          [sql| SELECT image_content FROM image WHERE image_id = ? |]
          (SQL.Only id') :: IO [SQL.Only String] --(SQL.toRow id)
      let rez = SQL.fromOnly . head $ res
      Logger.logDebug
        (News.hLogHandle h)
        (T.pack ("oneImage: OK! Get image with ID " ++ show id'))
      return $ Right $ read rez --  Base64.decodeBase64Lenient rez
    Left err -> return $ Left err

ceckId ::
     SQL.Connection
  -> (News.Handle IO, Integer)
  -> IO (Either ErrorTypes.GetImageError Integer)
ceckId conn (h', id') = do
  res <-
    SQL.query
      conn
      [sql| SELECT EXISTS (SELECT image_id  FROM image WHERE image_id = ?) |]
      (SQL.Only id') :: IO [SQL.Only Bool] --(SQL.toRow id)
  if SQL.fromOnly $ head res
    then do
      Logger.logDebug (News.hLogHandle h') $
        T.pack ("ceckId: OK! Image whith id " ++ show id' ++ " exists")
      return $ Right id'
    else do
      Logger.logError (News.hLogHandle h') $
        T.pack $
        show $
        ErrorTypes.InvalidImagedId $
        ErrorTypes.InvalidId
          ("ceckId: OK! Image whith id " ++ show id' ++ "not exists")
      return $ Left $ ErrorTypes.InvalidImagedId $ ErrorTypes.InvalidId []
