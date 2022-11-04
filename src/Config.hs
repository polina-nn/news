{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

-- | A module to provide a configuration reader for other modules.
module Config
  ( getAppConfig
  , getDbConfig
  , getLoggerConfig
  , getURIConfig
  ) where

import Control.Exception.Safe (throwString)
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import qualified Logger
import qualified Logger.Impl
import qualified News
import qualified System.IO

data MaybeDbConfig =
  MaybeDbConfig
    { maybeDbHost :: Maybe String
    , maybeDbName :: Maybe String
    , maybeUser :: Maybe String
    , maybePassword :: Maybe String
    , maybeDbPort :: Maybe String
    , maybeNoOfStripes :: Maybe Int
    , maybeIdleTime :: Maybe Int
    , maybeStripeSize :: Maybe Int
    }
  deriving (Show, Eq)

data MaybeAppConfig =
  MaybeAppConfig
    { maybeAppPort :: Maybe Int -- launch port
    , maybeAppShowLimit :: Maybe Int -- limit on the number of records in the server response
    }
  deriving (Show, Eq)

data MaybeURIConfig =
  MaybeURIConfig
    { maybeUriScheme :: Maybe String
    , maybeUriHost :: Maybe String
    , maybeUriPort :: Maybe Int
    }
  deriving (Show, Eq)

getURIConfig :: C.Config -> IO News.URIConfig
getURIConfig conf = do
  uriScheme <- C.lookup conf "config.uriScheme" :: IO (Maybe String)
  uriHost <- C.lookup conf "config.uriHost" :: IO (Maybe String)
  uriPort <- C.lookup conf "config.uriPort" :: IO (Maybe Int)
  let maybeURIConfig :: MaybeURIConfig
      maybeURIConfig = MaybeURIConfig uriScheme uriHost uriPort
  case checkURIConfig maybeURIConfig :: Maybe News.URIConfig of
    Nothing -> throwString "Error parsing configuration file for getURIConfig"
    Just val -> return val
  where
    checkURIConfig :: MaybeURIConfig -> Maybe News.URIConfig
    checkURIConfig (MaybeURIConfig (Just "Http") (Just uriHost) (Just uriPort)) =
      Just
        (News.URIConfig
           { News.uriScheme = News.Http
           , News.uriHost = uriHost
           , News.uriPort = uriPort
           })
    checkURIConfig (MaybeURIConfig (Just "Https") (Just uriHost) (Just uriPort)) =
      Just
        (News.URIConfig
           { News.uriScheme = News.Https
           , News.uriHost = uriHost
           , News.uriPort = uriPort
           })
    checkURIConfig _ = Nothing

-- | getAppConfig Try to read all values from the config, if it doesn't work, I send throwString
getAppConfig :: C.Config -> IO News.AppConfig
getAppConfig conf = do
  appPort <- C.lookup conf "config.appPort" :: IO (Maybe Int)
  appShowLimit <- C.lookup conf "config.appShowLimit" :: IO (Maybe Int)
  let maybeAppConfig :: MaybeAppConfig
      maybeAppConfig = MaybeAppConfig appPort appShowLimit
  case checkAppConfig maybeAppConfig :: Maybe News.AppConfig of
    Nothing -> throwString "Error parsing configuration file for getAppConfig"
    Just val -> return val
  where
    checkAppConfig :: MaybeAppConfig -> Maybe News.AppConfig
    checkAppConfig (MaybeAppConfig (Just appPort) (Just appShowLimit)) =
      Just $ News.AppConfig appPort appShowLimit
    checkAppConfig _ = Nothing

-- | getDbConfig Try to read all values from the config, if it doesn't work, I send throwString
getDbConfig :: C.Config -> IO News.DbConfig
getDbConfig conf = do
  dbHost <- C.lookup conf "config.dbHost" :: IO (Maybe String)
  dbName <- C.lookup conf "config.dbName" :: IO (Maybe String)
  user <- C.lookup conf "config.user" :: IO (Maybe String)
  password <- C.lookup conf "config.password" :: IO (Maybe String)
  dbPort <- C.lookup conf "config.dbPort" :: IO (Maybe String)
  noOfStripes <- C.lookup conf "config.noOfStripes" :: IO (Maybe Int)
  idleTime <- C.lookup conf "config.idleTime" :: IO (Maybe Int)
  stripeSize <- C.lookup conf "config.stripeSize" :: IO (Maybe Int)
  let maybeDbConfig :: MaybeDbConfig
      maybeDbConfig =
        MaybeDbConfig
          dbHost
          dbName
          user
          password
          dbPort
          noOfStripes
          idleTime
          stripeSize
  case checkDbConfig maybeDbConfig :: Maybe News.DbConfig of
    Nothing -> throwString "Error parsing configuration file for getDbConfig"
    Just val -> return val
  where
    checkDbConfig :: MaybeDbConfig -> Maybe News.DbConfig
    checkDbConfig (MaybeDbConfig (Just dbHost) (Just dbName) (Just user) (Just password) (Just dbPort) (Just noOfStripes) (Just idleTime) (Just stripeSize)) =
      Just
        (News.DbConfig
           { News.dbHost = dbHost
           , News.dbName = dbName
           , News.user = user
           , News.password = password
           , News.dbPort = dbPort
           , News.noOfStripes = noOfStripes
           , News.idleTime = idleTime
           , News.stripeSize = stripeSize
           })
    checkDbConfig _ = Nothing

-- | getLoggerConfig if there are problems with reading the logging config -> log to the console and the info level
getLoggerConfig :: C.Config -> IO Logger.Impl.Config
getLoggerConfig conf = do
  readStdError <- C.lookupDefault "C" conf "config.stdError" :: IO String
  readMinLogLevel <- C.lookupDefault "I" conf "config.minLogLevel" :: IO String
  confFileHandle <- validatefileHandle readStdError
  confMinLevel <- validateLogLevel readMinLogLevel
  return
    Logger.Impl.Config
      { Logger.Impl.confFileHandle = confFileHandle
      , Logger.Impl.confMinLevel = confMinLevel
      }

validatefileHandle :: String -> IO System.IO.Handle
validatefileHandle fileText =
  case fileText of
    "F" -> System.IO.openFile "logs" System.IO.AppendMode
    "C" -> return System.IO.stderr
    _ -> do
      putStrLn "validatefileHandle: stdError is invalid in config.conf file"
      return System.IO.stderr

validateLogLevel :: String -> IO Logger.Level
validateLogLevel levelText =
  case levelText of
    "E" -> return Logger.Error
    "W" -> return Logger.Warning
    "I" -> return Logger.Info
    "D" -> return Logger.Debug
    _ -> do
      putStrLn "validateLogLevel: minLogLevel  is invalid in config.conf file"
      return Logger.Info
{-- default config for my settings
getAppConfig :: C.Config -> IO News.AppConfig
getAppConfig c =
  return
    News.AppConfig
      { News.appPort = 8080, -- порт запуска
        News.appShowLimit = 30 -- лимит кол-ва записей в ответе  сервера
      }

getDbConfig :: C.Config -> IO News.DbConfig
getDbConfig c =
  return
    News.DbConfig
      { News.dbHost = "localhost",
        News.dbName = "tiny",
        News.user = "postgres",
        News.password = "postgres123",
        News.dbPort = "5432",
        News.noOfStripes = 2, --- stripes https://docs.servant.dev/en/stable/cookbook/db-postgres-pool/PostgresPool.html
        News.idleTime = 60, --- unused connections are kept open for a minute
        News.stripeSize = 10 --- max. 10 connections open per stripe
      }

getLoggerConfig :: C.Config -> IO Logger.Impl.Config
getLoggerConfig c =
  return
    Logger.Impl.Config
      { Logger.Impl.confFileHandle = System.IO.stderr,
        Logger.Impl.confMinLevel = Logger.Debug
      }

getURIConfig :: C.Config -> IO News.URIConfig
getURIConfig c =
  return
    News.URIConfig
     { -- | URI scheme to use
       News.uriScheme = News.Http,
    -- | host (eg "haskell.org")
        News.uriHost = "localhost",
    -- | port (eg 80)
       News.uriPort = 8080
      }
--}
