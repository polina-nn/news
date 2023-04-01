-- | Connection pool to DB used in module Server
module DbServices
  ( initConnPool,
    createDb,
    migrateDb,
  )
where

import qualified Control.Exception.Safe as EXS
import qualified Data.Pool as POOL
import qualified Database.PostgreSQL.Simple as SQL
import qualified Database.PostgreSQL.Simple.Migration as Migration
import qualified EndPoints.AddOneCategory as AddOneCategory
import qualified EndPoints.AddOneImage as AddOneImage
import qualified EndPoints.AddOneNews as AddOneNews
import qualified EndPoints.AddOneUser as AddOneUser
import qualified EndPoints.EditOneCategory as EditOneCategory
import qualified EndPoints.EditOneNews as EditOneNews
import qualified EndPoints.GetAuthorsNewsList as GetAuthorsNewsList
import qualified EndPoints.GetAuthorsNewsSearchList as GetAuthorsNewsSearchList
import qualified EndPoints.GetCategoryList as GetCategoryList
import qualified EndPoints.GetNewsList as GetNewsList
import qualified EndPoints.GetNewsSearchList as GetNewsSearchList
import qualified EndPoints.GetOneImage as GetOneImage
import qualified EndPoints.GetUserList as GetUserList
import Logger (logDebug, logError, (.<))
import qualified News
import qualified Types.DataTypes as DataTypes
import qualified Types.ExceptionTypes as ExceptionTypes

initConnPool :: DataTypes.Handle -> IO (POOL.Pool SQL.Connection)
initConnPool h = do
  --conn <- EXS.catch (tryInitConnectDb (DataTypes.hServerHandle h)) (ExceptionTypes.handleException (DataTypes.hServerHandle h))
  res <- EXS.try $ POOL.createPool (SQL.connectPostgreSQL $ SQL.postgreSQLConnectionString (url (DataTypes.hServerHandle h))) SQL.close noOfStripes' (realToFrac idleTime') stripeSize'
  case res of
    Left e -> do
      Logger.logError (News.hLogHandle (DataTypes.hServerHandle h)) "initConnPool: BAD, not connection to the Data Base"
      EXS.throw (ExceptionTypes.DbNotConnect e)
    Right conn -> do
      Logger.logDebug (News.hLogHandle (DataTypes.hServerHandle h)) "initConnPool: OK "
      pure conn
  where
    noOfStripes' = News.noOfStripes (News.hDbConfig (DataTypes.hServerHandle h))
    idleTime' = News.idleTime (News.hDbConfig (DataTypes.hServerHandle h))
    stripeSize' = News.stripeSize (News.hDbConfig (DataTypes.hServerHandle h))

{--
-- | tryInitConnectDb  - throws an exception if it was not possible to connect to the database, use in DbServices.initConnPool.
tryInitConnectDb :: News.Handle IO -> IO SQL.Connection
tryInitConnectDb h = do
  res <- EXS.try (SQL.connect (url h))
  case res of
    Left e -> do
      Logger.logError (News.hLogHandle h) "tryInitConnectDb: BAD, not connection to the Data Base"
      EXS.throw (ExceptionTypes.DbNotConnect e)
    Right conn -> do
      Logger.logDebug (News.hLogHandle h) "tryInitConnectDb: OK "
      pure conn --}

url :: News.Handle IO -> SQL.ConnectInfo
url h =
  SQL.ConnectInfo
    { connectHost = News.dbHost (News.hDbConfig h),
      connectPort = read $ News.dbPort (News.hDbConfig h),
      connectUser = News.user (News.hDbConfig h),
      connectPassword = News.password (News.hDbConfig h),
      connectDatabase = News.dbName (News.hDbConfig h)
    }

createDb :: POOL.Pool SQL.Connection -> DataTypes.Db
createDb pool =
  DataTypes.Db
    { dbAddUser = withConnPool . flip AddOneUser.addUser,
      dbAddCategory = withConnPool . flip AddOneCategory.addCategory,
      dbAddNews = withConnPool . flip AddOneNews.addNews,
      dbAddImage = withConnPool . flip AddOneImage.addImage,
      dbEditCategory = withConnPool . flip EditOneCategory.editCategory,
      dbEditNews = withConnPool . flip EditOneNews.editNews,
      dbAuthorsNewsList = withConnPool . flip GetAuthorsNewsList.authorsNewsList,
      dbAuthorsNewsSearchList =
        withConnPool . flip GetAuthorsNewsSearchList.authorsNewsSearchList,
      dbUserList = withConnPool . flip GetUserList.userList,
      dbOneImage = withConnPool . flip GetOneImage.oneImage,
      dbCategoryList = withConnPool . flip GetCategoryList.categoryList,
      dbNewsList = withConnPool . flip GetNewsList.newsList,
      dbNewsSearchList = withConnPool . flip GetNewsSearchList.newsSearchList
    }
  where
    withConnPool = POOL.withResource pool

migrateDb :: POOL.Pool SQL.Connection -> (News.Handle IO, String) -> IO ()
migrateDb pool (h, xs) = POOL.withResource pool $ \conn -> do
  initResult <-
    SQL.withTransaction conn . Migration.runMigration $
      Migration.MigrationContext Migration.MigrationInitialization True conn
  Logger.logDebug
    (News.hLogHandle h)
    ("migrateDb: MigrationInitialization " .< initResult)
  migrateResult <-
    SQL.withTransaction conn . Migration.runMigration $
      Migration.MigrationContext (Migration.MigrationDirectory xs) True conn
  Logger.logDebug
    (News.hLogHandle h)
    ("migrateDb: Migration result " .< migrateResult)
  case migrateResult of
    Migration.MigrationError err -> EXS.throwString err
    _ -> return ()
