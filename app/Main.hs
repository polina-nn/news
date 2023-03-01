module Main
  ( main,
  )
where

import qualified Config
import qualified Control.Exception as Exc
import Control.Exception.Base (throwIO)
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as CT
import qualified Logger
import qualified Logger.Impl
import qualified News
import qualified Server
import qualified Types.DataTypes as DataTypes

main :: IO ()
main = do
  loadedConf <-
    Exc.try $ C.load [C.Required "config.conf"] :: IO (Either Exc.IOException CT.Config)
  case loadedConf of
    Left exception -> do
      putStrLn $ "Did not load config file. Fault:  " ++ show exception
      throwIO exception
    Right conf -> do
      putStrLn "main: load config file, my news-server started"
      withLogHandle conf $ \logHandle -> do
        serverHandle <- makeServerHandle conf logHandle
        putStrLn "main: makeServerHandle: OК. ServerHandle created"
        runServer serverHandle

withLogHandle :: CT.Config -> (Logger.Handle IO -> IO ()) -> IO ()
withLogHandle conf f = do
  config <- Config.getLoggerConfig conf
  putStrLn "getLoggerConfig OK!"
  Logger.Impl.withHandle config f

runServer :: News.Handle IO -> IO ()
runServer serverHandle =
  Server.run DataTypes.Handle {DataTypes.hServerHandle = serverHandle}

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
