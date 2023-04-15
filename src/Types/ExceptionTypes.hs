{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.ExceptionTypes where

import qualified Control.Exception.Safe as EXS
import Logger (logError, (.<))
import qualified News
import qualified System.Exit as Exit

data ServerException
  = -- | DbNotConnect  -- ERROR when there is no database connection  (when trying to migrate)
    DbNotConnect EXS.SomeException
  | -- | MigrationError  -- ERROR throw during migrations ( when migration result not success)
    MigrationError String
  | -- | NotConfig -- ERROR throw when  did not load config file
    NotConfig EXS.SomeException
  deriving (EXS.Exception)

instance Show ServerException where
  show (DbNotConnect msg) = "Failed, not connection to the Data Base!  " ++ show msg
  show (MigrationError msg) = "Failed, during migrations!  " ++ show msg
  show (NotConfig msg) = "Failed, did not load config file! " ++ show msg

handleExceptionToLog :: News.Handle IO -> ServerException -> IO a
handleExceptionToLog h e = do
  Logger.logError (News.hLogHandle h) $ "catch ServerException: ERROR! " .< e
  Exit.exitFailure

handleExceptionToTerminal :: ServerException -> IO a
handleExceptionToTerminal e = do
  putStrLn $ "catch ServerException: ERROR! " ++ show e
  Exit.exitFailure
