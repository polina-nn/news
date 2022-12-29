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

module EndPoints.GetOneImage
  ( getOneImage,
    oneImage,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as SQL
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified DbException
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
  SQL.Connection ->
  (News.Handle IO, Integer) ->
  IO (Either ErrorTypes.GetImageError B.ByteString)
oneImage _ (h, id') = do
  rez <- DbException.tryRequestConnectDb h
  case rez of
    Left err -> return $ Left $ ErrorTypes.GetImageSQLRequestError err
    Right conn -> do
      Logger.logDebug (News.hLogHandle h) $ T.pack "Request: Get One Image"
      resCheckId <- checkId conn (h, id')
      case resCheckId of
        Right _ -> do
          res <-
            SQL.query
              conn
              [sql| SELECT image_content FROM image WHERE image_id = ? |]
              (SQL.Only id') ::
              IO [SQL.Only String]
          Logger.logDebug (News.hLogHandle h) (T.pack ("oneImage: OK! Get image with ID " ++ show id'))
          liftIO $ SQL.close conn
          return $ Right $ read (SQL.fromOnly . head $ res)
        Left err -> do
          liftIO $ SQL.close conn
          return $ Left err

checkId ::
  SQL.Connection ->
  (News.Handle IO, Integer) ->
  IO (Either ErrorTypes.GetImageError Integer)
checkId conn (h', id') = do
  res <-
    SQL.query
      conn
      [sql| SELECT EXISTS (SELECT image_id  FROM image WHERE image_id = ?) |]
      (SQL.Only id') ::
      IO [SQL.Only Bool] --(SQL.toRow id)
  if SQL.fromOnly $ head res
    then do
      Logger.logDebug (News.hLogHandle h') $
        T.pack ("checkId: OK! Image with id " ++ show id' ++ " exists")
      return $ Right id'
    else do
      Logger.logError (News.hLogHandle h') $
        T.pack $
          show $
            ErrorTypes.InvalidImagedId $
              ErrorTypes.InvalidId
                ("checkId: OK! Image with id " ++ show id' ++ "not exists")
      return $ Left $ ErrorTypes.InvalidImagedId $ ErrorTypes.InvalidId []
