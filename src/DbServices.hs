{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Connection pool to DB used in module Server
module DbServices
  ( initConnPool,
    createDb,
    migrateDb,
  )
where

import qualified Control.Exception.Safe as EX
import qualified Data.Pool as POOL
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as SQL
import qualified Database.PostgreSQL.Simple.Migration as Migration
import qualified DbException
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
import qualified Logger
import qualified News
import qualified Types.DataTypes as DataTypes

initConnPool :: DataTypes.Handle -> IO (POOL.Pool SQL.Connection)
initConnPool h = do
  conn <- EX.catch (DbException.tryInitConnectDb (DataTypes.hServerHandle h)) (DbException.handleException (DataTypes.hServerHandle h))
  POOL.createPool (pure conn) SQL.close noOfStripes' (realToFrac idleTime') stripeSize'
  where
    noOfStripes' = News.noOfStripes (News.hDbConfig (DataTypes.hServerHandle h))
    idleTime' = News.idleTime (News.hDbConfig (DataTypes.hServerHandle h))
    stripeSize' = News.stripeSize (News.hDbConfig (DataTypes.hServerHandle h))

createDb :: POOL.Pool SQL.Connection -> DataTypes.Db
createDb pool =
  DataTypes.Db
    { _addUser = withConnPool . flip AddOneUser.addUser,
      _addCategory = withConnPool . flip AddOneCategory.addCategory,
      _addNews = withConnPool . flip AddOneNews.addNews,
      _addImage = withConnPool . flip AddOneImage.addImage,
      _editCategory = withConnPool . flip EditOneCategory.editCategory,
      _editNews = withConnPool . flip EditOneNews.editNews,
      _authorsNewsList = withConnPool . flip GetAuthorsNewsList.authorsNewsList,
      _authorsNewsSearchList =
        withConnPool . flip GetAuthorsNewsSearchList.authorsNewsSearchList,
      _userList = withConnPool . flip GetUserList.userList,
      _oneImage = withConnPool . flip GetOneImage.oneImage,
      _categoryList = withConnPool . flip GetCategoryList.categoryList,
      _newsList = withConnPool . flip GetNewsList.newsList,
      _newsSearchList = withConnPool . flip GetNewsSearchList.newsSearchList
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
    (T.pack ("migrateDb: MigrationInitialization " ++ show initResult))
  migrateResult <-
    SQL.withTransaction conn . Migration.runMigration $
      Migration.MigrationContext (Migration.MigrationDirectory xs) True conn
  Logger.logDebug
    (News.hLogHandle h)
    (T.pack ("migrateDb: Migration result " ++ show migrateResult))
