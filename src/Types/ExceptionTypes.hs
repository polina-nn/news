{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.ExceptionTypes where

import qualified Control.Exception.Safe as EX
import qualified Data.Text as T
import qualified Logger
import qualified News
import qualified System.Exit as Exit

newtype DbException
  = -- | DbNotConnect  -- ERROR when there is no database connection
    DbNotConnect EX.SomeException
  deriving (EX.Exception)

instance Show DbException where
  show (DbNotConnect msg) = "Failed, not connection to the Data Base!  " ++ show msg

handleException :: News.Handle IO -> EX.SomeException -> IO a
handleException h (EX.SomeException e) = do
  Logger.logError (News.hLogHandle h) $ T.concat ["catch SomeException: ERROR! ", T.pack $ show e]
  Exit.exitFailure
