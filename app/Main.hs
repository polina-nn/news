module Main
  ( main,
  )
where

import qualified Config
import qualified Control.Exception as Exc
import Control.Exception.Base (throwIO)
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import qualified Logger
import qualified Logger.Impl
import qualified News
import qualified Server

main :: IO ()
main = do
  loadedConf <-
    Exc.try $ C.load [C.Required "config.conf"] :: IO (Either Exc.IOException C.Config)
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

withLogHandle :: C.Config -> (Logger.Handle IO -> IO ()) -> IO ()
withLogHandle conf f = do
  config <- Config.getLoggerConfig conf
  putStrLn "getLoggerConfig OK!"
  Logger.Impl.withHandle config f

runServer :: News.Handle IO -> IO ()
runServer serverHandle =
  Server.run Server.Handle {Server.hServerHandle = serverHandle}

makeServerHandle :: C.Config -> Logger.Handle IO -> IO (News.Handle IO)
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

{--
main :: IO ()
main = do
  args <- getArgs
  AppConfig {..} <- loadAppConfig args
  pool <- DbServices.initConnPool dbConfig
  withResource pool (`DbServices.migrateDb` args)
  run appPort $ app pool

app :: Pool Connection -> Application
app = serve serviceApi . server . DbServices.createDb

migrateDb :: Connection -> [String] -> IO ()
migrateDb conn xs =
  do
    initResult <-
      withTransaction conn . runMigration $
        MigrationContext MigrationInitialization True conn
    print $ "MigrationInitialization: " ++ show initResult
    migrateResult <-
      withTransaction conn . runMigration $
        MigrationContext (MigrationDirectory dir) True conn
    print $ "Migration result: " ++ show migrateResult
  where
    dir = resourceFolder xs ++ "/dbscripts"

resourceFolder :: [String] -> String
resourceFolder [] = "src/resources"
resourceFolder (folder : _) = folder

AppConfig {
  appPort = 9001
  DbConfig {
    dbHost = "$(DB_HOST)"
    dbName = "$(DB_NAME)"
    user = "$(DB_USER)"
    password = "$(DB_PASSWORD)"
    dbPort = "$(DB_PORT)"
    noOfStripes = 2
    idleTime = 60
    stripeSize = 10
  }
}

--}