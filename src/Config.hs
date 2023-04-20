-- | A module to provide a configuration reader for other modules.
module Config
  ( getAppConfig,
    getDbConfig,
    getLoggerConfig,
    getURIConfig,
    tryLoadConfig,
  )
where

import qualified Control.Exception.Safe as EXS
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import qualified Data.Text as T
import qualified Logger
import qualified Logger.Impl
import qualified News
import qualified System.Directory as SD
import qualified System.IO
import Text.Read (readMaybe)
import qualified Types.ExceptionTypes as ExceptionTypes

-- | StdError  - use for choice in config. (I expect to see in the config.conf the words Terminal or File in stdError field)
data StdError = Terminal | File
  deriving (Show, Eq, Ord, Read)

instance C.Configured StdError where
  convert (C.String str) = readMaybe (T.unpack str)
  convert _ = Nothing

-- | FieldName -  the name of a Config Field
type FieldName = T.Text

tryLoadConfig :: FilePath -> IO C.Config
tryLoadConfig path = do
  loadedConf <-
    EXS.try $ C.load [C.Required path] :: IO (Either EXS.SomeException C.Config)
  case loadedConf of
    Left exception -> EXS.throw (ExceptionTypes.NotConfig exception)
    Right conf -> return conf

tryLookUpConfigIntValueField :: C.Config -> FieldName -> IO Int
tryLookUpConfigIntValueField conf name = do
  rez <- C.lookup conf name
  case rez of
    Nothing -> EXS.throw (ExceptionTypes.NotConfigField name)
    Just value -> return value

tryLookUpConfigStringValueField :: C.Config -> FieldName -> IO String
tryLookUpConfigStringValueField conf name = do
  rez <- C.lookup conf name
  case rez of
    Nothing -> EXS.throw (ExceptionTypes.NotConfigField name)
    Just value -> return value

getLoggerConfig :: C.Config -> IO Logger.Impl.Config
getLoggerConfig conf = do
  fieldValueStdError <- C.lookup conf "config.stdError"
  fieldValueMinLogLevel <- C.lookup conf "config.minLogLevel"
  confFileHandle <- EXS.catch (validateFileHandle fieldValueStdError) ExceptionTypes.handleExceptionToTerminal
  confFieldValueMinLogLevel <- EXS.catch (validateLevel fieldValueMinLogLevel) ExceptionTypes.handleExceptionToTerminal
  return
    Logger.Impl.Config
      { Logger.Impl.confFileHandle = confFileHandle,
        Logger.Impl.confMinLevel = confFieldValueMinLogLevel
      }

validateFileHandle :: Maybe StdError -> IO System.IO.Handle
validateFileHandle (Just Terminal) = return System.IO.stderr
validateFileHandle (Just File) = appendLog "logs.txt"
validateFileHandle Nothing = EXS.throw (ExceptionTypes.NotConfigField "config.stdError")

validateLevel :: Maybe Logger.Level -> IO Logger.Level
validateLevel (Just val) = return val
validateLevel Nothing = EXS.throw (ExceptionTypes.NotConfigField "config.minLogLevel")

-- | appendLog  - check the existence of the file, if it does't  exist, create and append
appendLog :: FilePath -> IO System.IO.Handle
appendLog path = do
  rez <- SD.doesFileExist path
  if rez
    then System.IO.openFile "logs.txt" System.IO.AppendMode
    else do
      putStrLn "Create the file /logs.txt"
      System.IO.writeFile "logs.txt" []
      System.IO.openFile "logs.txt" System.IO.AppendMode

getAppConfig :: C.Config -> IO News.AppConfig
getAppConfig conf = do
  appPort' <- EXS.catch (tryLookUpConfigIntValueField conf "config.appPort") ExceptionTypes.handleExceptionToTerminal
  appShowLimit' <- EXS.catch (tryLookUpConfigIntValueField conf "config.appShowLimit") ExceptionTypes.handleExceptionToTerminal
  return $ News.AppConfig {appPort = appPort', appShowLimit = appShowLimit'}

getDbConfig :: C.Config -> IO News.DbConfig
getDbConfig conf = do
  dbHost' <- EXS.catch (tryLookUpConfigStringValueField conf "config.dbHost") ExceptionTypes.handleExceptionToTerminal
  dbName' <- EXS.catch (tryLookUpConfigStringValueField conf "config.dbName") ExceptionTypes.handleExceptionToTerminal
  user' <- EXS.catch (tryLookUpConfigStringValueField conf "config.user") ExceptionTypes.handleExceptionToTerminal
  password' <- EXS.catch (tryLookUpConfigStringValueField conf "config.password") ExceptionTypes.handleExceptionToTerminal
  dbPort' <- EXS.catch (tryLookUpConfigStringValueField conf "config.dbPort") ExceptionTypes.handleExceptionToTerminal
  noOfStripes' <- EXS.catch (tryLookUpConfigIntValueField conf "config.noOfStripes") ExceptionTypes.handleExceptionToTerminal
  idleTime' <- EXS.catch (tryLookUpConfigIntValueField conf "config.idleTime") ExceptionTypes.handleExceptionToTerminal
  stripeSize' <- EXS.catch (tryLookUpConfigIntValueField conf "config.stripeSize") ExceptionTypes.handleExceptionToTerminal
  return $
    News.DbConfig
      { dbHost = dbHost',
        dbName = dbName',
        user = user',
        password = password',
        dbPort = dbPort',
        noOfStripes = noOfStripes',
        idleTime = idleTime',
        stripeSize = stripeSize'
      }

getURIConfig :: C.Config -> IO News.URIConfig
getURIConfig conf = do
  uriScheme' <- C.lookup conf "config.uriScheme"
  uriHost' <- EXS.catch (tryLookUpConfigStringValueField conf "config.uriHost") ExceptionTypes.handleExceptionToTerminal
  uriPort' <- EXS.catch (tryLookUpConfigIntValueField conf "config.uriPort") ExceptionTypes.handleExceptionToTerminal
  confFieldValueUriScheme <- EXS.catch (validateUriScheme uriScheme') ExceptionTypes.handleExceptionToTerminal
  return $
    News.URIConfig
      { uriScheme = confFieldValueUriScheme,
        uriHost = uriHost',
        uriPort = uriPort'
      }

validateUriScheme :: Maybe News.Scheme -> IO News.Scheme
validateUriScheme (Just val) = return val
validateUriScheme Nothing = EXS.throw (ExceptionTypes.NotConfigField "config.minLogLevel")
