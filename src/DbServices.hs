-- | Connection pool to DB used in module Server
module DbServices
  ( initConnPool,
    createDb,
    migrateDb,
  )
where

import qualified Data.ByteString.Char8 as BS8
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
import Logger (logDebug, (.<))
import qualified News
import qualified Types.DataTypes as DataTypes

initConnPool :: News.DbConfig -> IO (POOL.Pool SQL.Connection)
initConnPool News.DbConfig {..} =
  POOL.createPool
    (SQL.connectPostgreSQL url)
    SQL.close
    noOfStripes
    (realToFrac idleTime)
    stripeSize
  where
    url =
      BS8.pack $
        "host='"
          ++ dbHost
          ++ "' dbname='"
          ++ dbName
          ++ "' user='"
          ++ user
          ++ "' password='"
          ++ password
          ++ "' port="
          ++ dbPort

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
