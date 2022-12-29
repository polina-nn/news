-- it is necessary for Servant
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Server
  ( server,
    serviceApi,
    run,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.Pool as POOL
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as SQL
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified DbException
import qualified DbServices
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
import qualified EndPoints.Lib.Lib as Lib
import qualified Logger
import qualified Network.Wai.Handler.Warp
import qualified News
import Servant
  ( Application,
    BasicAuthCheck (..),
    BasicAuthData (..),
    BasicAuthResult (..),
    Context (EmptyContext, (:.)),
    Proxy (..),
    Server,
    serveWithContext,
    (:<|>) ((:<|>)),
  )
import qualified Types.ApiTypes as ApiTypes
import qualified Types.DataTypes as DataTypes

server :: News.Handle IO -> DataTypes.Db -> Server ApiTypes.RestAPI
server h db =
  return (T.pack "Welcome to tiny news server") :<|> AddOneUser.addOneUser h db
    :<|> AddOneCategory.addOneCategory h db
    :<|> AddOneNews.addOneNews h db
    :<|> AddOneImage.addOneImage h db
    :<|> EditOneCategory.editOneCategory h db
    :<|> EditOneNews.editOneNews h db
    :<|> GetAuthorsNewsList.getAuthorsNewsList h db
    :<|> GetAuthorsNewsSearchList.getAuthorsNewsSearchList h db
    :<|> GetUserList.getUserList h db
    :<|> GetOneImage.getOneImage h db
    :<|> GetCategoryList.getCategoryList h db
    :<|> GetNewsList.getNewsList h db
    :<|> GetNewsSearchList.getNewsSearchList h db

serviceApi :: Proxy ApiTypes.RestAPI
serviceApi = Proxy

run :: DataTypes.Handle -> IO ()
run h = do
  Logger.logDebug (News.hLogHandle (DataTypes.hServerHandle h)) "run: Server is running"
  let appConfig = News.hAppConfig (DataTypes.hServerHandle h)
  let appPort = News.appPort appConfig
  pool <- DbServices.initConnPool h
  POOL.withResource
    pool
    (`DbServices.migrateDb` (DataTypes.hServerHandle h, "_migrations"))
  Network.Wai.Handler.Warp.run appPort $ app (DataTypes.hServerHandle h) pool

app :: News.Handle IO -> POOL.Pool SQL.Connection -> Application
app h connPool = do
  serveWithContext Server.serviceApi ctx $
    Server.server h $ DbServices.createDb connPool
  where
    ctx = BasicAuthCheck authCfg :. EmptyContext
    authCfg = myAuthCheck h connPool

-- | myAuthCheck function is the one that actually check the username
-- and password and return an value that indicate the status of authentication. Look in the 'app' function
-- to see how it is used. The value returned can be one of: Unauthorized, BadPassword, NoSuchUser, Authorized usr
myAuthCheck ::
  News.Handle IO ->
  POOL.Pool SQL.Connection ->
  BasicAuthData ->
  IO (BasicAuthResult DataTypes.User)
myAuthCheck h _ (BasicAuthData u p) = do
  tryConnectDb <- DbException.tryRequestConnectDb h
  case tryConnectDb of
    Left _ -> do
      Logger.logInfo
        (News.hLogHandle h)
        "myAuthCheck: Don't connect to Data Base. Unauthorized "
      return Unauthorized
    Right conn -> do
      conns <- POOL.createPool (pure conn) SQL.close noOfStripes' (realToFrac idleTime') stripeSize' :: IO (POOL.Pool SQL.Connection)
      isSuchUser <- POOL.withResource conns . flip suchUser $ u
      if isSuchUser == 0
        then do
          Logger.logInfo
            (News.hLogHandle h)
            "myAuthCheck: suchUser: user not found! NoSuchUser "
          return NoSuchUser
        else do
          isGoodPassword <- POOL.withResource conns . flip goodPassword $ (u, p)
          if isGoodPassword == 0
            then do
              Logger.logInfo
                (News.hLogHandle h)
                "myAuthCheck: goodPassword: user found, but his password not valid! BadPassword "
              return BadPassword
            else do
              users <- POOL.withResource conns . flip userAuthorized $ u
              let a = head users
              Logger.logInfo
                (News.hLogHandle h)
                "myAuthCheck: Ok!  Authorized "
              return $ Authorized a
  where
    noOfStripes' = News.noOfStripes (News.hDbConfig h)
    idleTime' = News.idleTime (News.hDbConfig h)
    stripeSize' = News.stripeSize (News.hDbConfig h)

userAuthorized :: SQL.Connection -> ByteString -> IO [DataTypes.User]
userAuthorized conn login = do
  res <-
    SQL.query
      conn
      [sql| SELECT usr_name, usr_login, usr_admin, usr_author, usr_created FROM usr WHERE usr_login= ? |]
      (SQL.Only login)
  return $ Prelude.map Lib.toUser res

-- | suchUser. If the query returned one value, then the user will find it, otherwise no.
-- Because logins are not repeated in the database
suchUser :: SQL.Connection -> ByteString -> IO Int
suchUser conn login = do
  res <-
    SQL.query
      conn
      [sql| SELECT COUNT (usr_login) FROM usr WHERE usr_login= ? |]
      (SQL.Only login)
  case res of
    [SQL.Only m] -> return m
    _ -> return 0

-- | goodPassword. If the query returned one value, then password is good, otherwise no.
-- Good password it means that the entered password equals the password in the database.
goodPassword :: SQL.Connection -> (ByteString, ByteString) -> IO Int
goodPassword conn (login, password) = do
  let hashedPassword = BSC8.pack $ Lib.hashed $ BSC8.unpack password
  res <-
    SQL.query
      conn
      [sql| SELECT COUNT (usr_login) FROM usr WHERE usr_login= ? AND usr_password= ? |]
      (login, hashedPassword)
  case res of
    [SQL.Only n] -> return n
    _ -> return 0
