{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module DbException (tryInitConnectDb, tryRequestConnectDb, handleException) where

import qualified Control.Exception.Safe as EX
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as SQL
import qualified Logger
import qualified News
import qualified System.Exit as Exit
import qualified Types.ErrorTypes as ErrorTypes

newtype DbException
  = -- | DbNotConnect  -- ERROR when there is no database connection
    DbNotConnect EX.SomeException
  deriving (EX.Exception)

instance Show DbException where
  show (DbNotConnect msg) = "Failed, not connection to the Data Base!  " ++ show msg

handleException :: News.Handle IO -> EX.SomeException -> IO a
handleException h (EX.SomeException e) = do
  Logger.logError (News.hLogHandle h) $
    T.pack ("catch SomeException: ERROR! " ++ show e)
  Exit.exitFailure

-- | tryInitConnectDb  - throws an exception if it was not possible to connect to the database, use in DbServices.initConnPool.
tryInitConnectDb :: News.Handle IO -> IO SQL.Connection
tryInitConnectDb h = do
  res <- EX.try (SQL.connect (url h))
  case res of
    Left e -> do
      Logger.logDebug (News.hLogHandle h) "tryConnectDataBase: BAD, not connection to the Data Base"
      EX.throw (DbNotConnect e)
    Right conn -> do
      Logger.logDebug (News.hLogHandle h) "tryConnectDataBase: OK "
      pure conn

-- | tryRequestConnectDb - Use it in each endpoints (before request), if the connection to the database failed, I send an SQLRequestError
tryRequestConnectDb :: News.Handle IO -> IO (Either ErrorTypes.SQLRequestError SQL.Connection)
tryRequestConnectDb h = do
  res <- EX.try (SQL.connect (url h)) :: IO (Either EX.SomeException SQL.Connection)
  case res of
    Left _ -> do
      Logger.logDebug (News.hLogHandle h) "tryConnectDataBase: BAD, not connection to Data Base"
      return $ Left $ ErrorTypes.SQLRequestError []
    Right conn -> do
      Logger.logDebug (News.hLogHandle h) "tryConnectDataBase: OK "
      return $ Right conn

url :: News.Handle IO -> SQL.ConnectInfo
url h =
  SQL.ConnectInfo
    { connectHost = News.dbHost (News.hDbConfig h),
      connectPort = read $ News.dbPort (News.hDbConfig h),
      connectUser = News.user (News.hDbConfig h),
      connectPassword = News.password (News.hDbConfig h),
      connectDatabase = News.dbName (News.hDbConfig h)
    }
