-- | Connection pool to DB used in module Server
module DbServices
  ( initConnPool,
    createDb,
    migrate,
  )
where

import qualified Control.Exception.Safe as EXS
import qualified Data.Aeson as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.Pool as POOL
import Data.String
import qualified Database.PostgreSQL.Simple as SQL
import qualified Database.PostgreSQL.Simple.Migration as Migration
import qualified DbConnect
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
import Logger (logDebug, (.<))
import qualified News
import qualified Types.DataTypes as DataTypes
import qualified Types.ExceptionTypes as ExceptionTypes

initConnPool :: DataTypes.Handle -> IO DataTypes.StatePool
initConnPool h = do
  POOL.createPool (SQL.connectPostgreSQL $ SQL.postgreSQLConnectionString (url h)) SQL.close noOfStripes' (realToFrac idleTime') stripeSize'
  where
    noOfStripes' = News.noOfStripes (News.hDbConfig (DataTypes.hServerHandle h))
    idleTime' = News.idleTime (News.hDbConfig (DataTypes.hServerHandle h))
    stripeSize' = News.stripeSize (News.hDbConfig (DataTypes.hServerHandle h))

url :: DataTypes.Handle -> SQL.ConnectInfo
url h =
  SQL.ConnectInfo
    { connectHost = News.dbHost (News.hDbConfig (DataTypes.hServerHandle h)),
      connectPort = read $ News.dbPort (News.hDbConfig (DataTypes.hServerHandle h)),
      connectUser = News.user (News.hDbConfig (DataTypes.hServerHandle h)),
      connectPassword = News.password (News.hDbConfig (DataTypes.hServerHandle h)),
      connectDatabase = News.dbName (News.hDbConfig (DataTypes.hServerHandle h))
    }

createDb :: DataTypes.StatePool -> DataTypes.Db
createDb pool =
  DataTypes.Db
    { dbAddUser = AddOneUser.addUser pool,
      dbAddCategory = AddOneCategory.addCategory pool,
      dbAddNews = AddOneNews.addNews pool,
      dbAddImage = AddOneImage.addImage pool,
      dbEditCategory = EditOneCategory.editCategory pool,
      dbEditNews = EditOneNews.editNews pool,
      dbAuthorsNewsList = GetAuthorsNewsList.authorsNewsList pool,
      dbAuthorsNewsSearchList =
        GetAuthorsNewsSearchList.authorsNewsSearchList pool,
      dbUserList = GetUserList.userList pool,
      dbOneImage = GetOneImage.oneImage pool,
      dbCategoryList = GetCategoryList.categoryList pool,
      dbNewsList = GetNewsList.newsList pool,
      dbNewsSearchList = GetNewsSearchList.newsSearchList pool
    }

migrate :: DataTypes.StatePool -> (News.Handle IO, String) -> IO ()
migrate pool (h, xs) = POOL.withResource pool $ \conn -> do
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

{--
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

migrateDb :: SQL.Connection -> (News.Handle IO, String) -> IO ()
migrateDb conn (h, xs) = do
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

    --}
