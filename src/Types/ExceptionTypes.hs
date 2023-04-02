{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.ExceptionTypes where

import qualified Control.Exception.Safe as EXS
import qualified Data.Text as T
import qualified Logger
import qualified News
import qualified System.Exit as Exit

data ServerException
  = -- | DbNotConnect  -- ERROR when there is no database connection
    DbNotConnect EXS.SomeException
  | -- | MigrationError  -- ERROR throw during migrations
    MigrationError String
  deriving (EXS.Exception)

instance Show ServerException where
  show (DbNotConnect msg) = "Failed, not connection to the Data Base!  " ++ show msg
  show (MigrationError msg) = "Failed, during migrations  " ++ show msg

handleException :: News.Handle IO -> EXS.SomeException -> IO a
handleException h (EXS.SomeException e) = do
  Logger.logError (News.hLogHandle h) $ T.concat ["catch ServerException: ERROR! ", T.pack $ show e]
  Exit.exitFailure
