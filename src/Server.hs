module Server
  ( server,
    serviceApi,
    run,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Control.Monad.Trans.Except as EX
import Data.ByteString (ByteString)
import qualified Data.Pool as POOL
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as SQL
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
import qualified EndPoints.Lib.LibIO as LibIO
import Logger (logDebug, logError, (.<))
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
  DbServices.migrate pool (DataTypes.hServerHandle h, "_migrations")
  Network.Wai.Handler.Warp.run appPort $ app (DataTypes.hServerHandle h) pool

app :: News.Handle IO -> DataTypes.StatePool -> Application
app h pool =
  serveWithContext Server.serviceApi genAuthServerContext $ Server.server h $ DbServices.createDb pool
  where
    genAuthServerContext :: Context (AuthHandler Request DataTypes.Account ': '[])
    genAuthServerContext = authHandler :. EmptyContext
    authHandler = authHandlerConn h pool

authHandlerConn ::
  News.Handle IO ->
  POOL.Pool SQL.Connection ->
  AuthHandler Request DataTypes.Account
authHandlerConn h conn = mkAuthHandler handler
  where
    maybeToEither e = maybe (Left e) Right
    throw401 msg = throwError $ err401 {errBody = msg}
    handler :: Request -> Handler DataTypes.Account
    handler req = either throw401 (lookupAccount h conn) $ do
      cookie <- maybeToEither "Missing cookie header" $ lookup "cookie" $ requestHeaders req
      maybeToEither "Missing token in cookie" $ lookup "servant-auth-cookie" $ parseCookies cookie

lookupAccount :: News.Handle IO -> DataTypes.StatePool -> ByteString -> Handler DataTypes.Account
lookupAccount h pool key = do
  account <- liftIO $ EX.runExceptT $ POOL.withResource pool . flip LibIO.searchAccount $ (h, key)
  case account of
    Left (ErrorTypes.ServerAuthErrorSQLRequestError a) -> throwError err500 {errReasonPhrase = show a}
    Left (ErrorTypes.ServerAuthErrorInvalidCookie a) -> throwError err403 {errReasonPhrase = show a}
    Right acc -> do
      liftIO $ Logger.logDebug (News.hLogHandle h) $ "lookupAccount = " .< acc
      return acc
