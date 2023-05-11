module Main
  ( main,
  )
where

import qualified Config
import qualified Control.Exception.Safe as EXS
import qualified Data.Configurator.Types as CT
import qualified Logger
import qualified Logger.Impl
import qualified News
import qualified Server
import qualified System.IO
import qualified Types.DataTypes as DataTypes
import qualified Types.ExceptionTypes as ExceptionTypes

main :: IO ()
main = do
  conf <- EXS.catch (Config.tryLoadConfig "config.conf") ExceptionTypes.handleExceptionToTerminal
  putStrLn "main: load config file, my news-server started"
  loggerImplConfig <- Config.getLoggerConfig conf
  putStrLn "getLoggerConfig OK!"
  withLogHandle loggerImplConfig $ \logHandle -> do
    serverHandle <- makeServerHandle conf logHandle
    putStrLn "main: makeServerHandle: OК. ServerHandle created"
    runServer serverHandle loggerImplConfig

withLogHandle :: Logger.Impl.Config -> (Logger.Handle IO -> IO ()) -> IO ()
withLogHandle = Logger.Impl.withHandle

runServer :: News.Handle IO -> Logger.Impl.Config -> IO ()
runServer serverHandle conf = EXS.finally (Server.run DataTypes.Handle {DataTypes.hServerHandle = serverHandle}) (System.IO.hClose $ Logger.Impl.confFileHandle conf)

makeServerHandle :: CT.Config -> Logger.Handle IO -> IO (News.Handle IO)
makeServerHandle conf logHandle = do
  appConfig <- Config.getAppConfig conf
  putStrLn "getAppConfig OK!"
  dbConfig <- Config.getDbConfig conf
  putStrLn "getDbConfig OK!"
  uriConfig <- Config.getURIConfig conf
  putStrLn "getURIConfig OK!"
  pure
    News.Handle
      { News.hLogHandle = logHandle,
        News.hAppConfig = appConfig,
        News.hDbConfig = dbConfig,
        News.hURIConfig = uriConfig
      }
