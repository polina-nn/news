module Server
  ( server,
    serviceApi,
    run,
  )
where

import qualified Control.Exception.Safe as EXS
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Control.Monad.Trans.Except as EX
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.Pool as POOL
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as SQL
import Database.PostgreSQL.Simple.SqlQQ (sql)
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
import qualified EndPoints.Lib.ThrowError as Throw
import Logger (logDebug, logError)
import Network.Wai (Request, requestHeaders)
import qualified Network.Wai.Handler.Warp
import qualified News
import Servant
  ( Application,
    Context (EmptyContext, (:.)),
    Handler,
    Proxy (..),
    Server,
    err401,
    err403,
    err500,
    errBody,
    errReasonPhrase,
    serveWithContext,
    throwError,
    (:<|>) ((:<|>)),
  )
import Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler) -- AuthServerData,
import qualified Types.ApiTypes as ApiTypes
import qualified Types.DataTypes as DataTypes
import qualified Types.ErrorTypes as ErrorTypes
import qualified Types.ExceptionTypes as ExceptionTypes
import Web.Cookie (parseCookies)

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
  EXS.catch (DbServices.migrateDb pool (DataTypes.hServerHandle h, "_migrations")) (ExceptionTypes.handleExceptionToLog (DataTypes.hServerHandle h))
  Network.Wai.Handler.Warp.run appPort $ app (DataTypes.hServerHandle h) pool

app :: News.Handle IO -> POOL.Pool SQL.Connection -> Application
app h connPool =
  serveWithContext Server.serviceApi genAuthServerContext $ Server.server h $ DbServices.createDb connPool
  where
    genAuthServerContext :: Context (AuthHandler Request DataTypes.Token ': '[])
    genAuthServerContext = authHandler :. EmptyContext
    authHandler = authHandlerConn h connPool

authHandlerConn ::
  News.Handle IO ->
  POOL.Pool SQL.Connection ->
  AuthHandler Request DataTypes.Token
authHandlerConn h conn = mkAuthHandler handler
  where
    maybeToEither e = maybe (Left e) Right
    throw401 msg = throwError $ err401 {errBody = msg}
    handler :: Request -> Handler DataTypes.Token
    handler req = either throw401 (lookupToken h conn) $ do
      cookie <- maybeToEither "Missing cookie header" $ lookup "cookie" $ requestHeaders req
      maybeToEither "Missing token in cookie" $ lookup "servant-auth-cookie" $ parseCookies cookie

lookupToken :: News.Handle IO -> POOL.Pool SQL.Connection -> ByteString -> Handler DataTypes.Token
lookupToken h pool key = do
  token <- liftIO $ EX.runExceptT $ lookupTokenDB pool (h, key)
  case token of
    Left (ErrorTypes.ServerAuthErrorSQLRequestError _) -> throwError err500 {errReasonPhrase = ErrorTypes.error500}
    Left (ErrorTypes.ServerAuthSomeException _) -> throwError err500 {errReasonPhrase = ErrorTypes.error500}
    Left (ErrorTypes.ServerAuthErrorInvalidToken _) -> throwError err403 {errReasonPhrase = ErrorTypes.error403}
    Right value -> return value

lookupTokenDB :: POOL.Pool SQL.Connection -> (News.Handle IO, ByteString) -> EX.ExceptT ErrorTypes.ServerAuthError IO DataTypes.Token
lookupTokenDB pool (h, t) = do
  let token = Lib.hashed $ BSC8.unpack t
  res <-
    liftIO
      ( EXS.try
          ( POOL.withResource pool $ \conn ->
              SQL.query
                conn
                [sql| SELECT EXISTS  (SELECT  token_key FROM token WHERE token_key = ?) |]
                (SQL.Only token)
          ) ::
          IO (Either EXS.SomeException [SQL.Only Bool])
      )
  case res of
    Left err -> Throw.throwSomeException h "lookupTokenDB" err
    Right [SQL.Only True] -> do
      liftIO $ Logger.logDebug (News.hLogHandle h) " lookupTokenDB: OK! Token exist "
      return DataTypes.Token {..}
    Right [SQL.Only False] -> do
      liftIO $ Logger.logError (News.hLogHandle h) "ERROR: lookupTokenDB: OK! Token not exist "
      EX.throwE $ ErrorTypes.ServerAuthErrorInvalidToken ErrorTypes.InvalidToken
    Right _ -> Throw.throwSqlRequestError h "lookupTokenDB" (ErrorTypes.SQLRequestError "Developer error!")
