module EndPoints.GetOneImage
  ( getOneImage,
    oneImage,
  )
where

import qualified Control.Exception.Safe as EXS
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Control.Monad.Trans.Except as EX
import qualified Data.ByteString as B
import qualified Data.Pool as POOL
import qualified Database.PostgreSQL.Simple as SQL
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified EndPoints.Lib.ToHttpResponse as ToHttpResponse
import Logger (logDebug, logInfo, (.<))
import qualified News
import Servant (Handler)
import qualified Text.Read as R
import qualified Types.DataTypes as DataTypes
import qualified Types.ErrorTypes as ErrorTypes

getOneImage :: News.Handle IO -> DataTypes.Db -> DataTypes.Id DataTypes.Image -> Handler B.ByteString
getOneImage h DataTypes.Db {..} idImage =
  (>>=) (liftIO $ dbOneImage (h, idImage)) ToHttpResponse.toHttpResponse

oneImage ::
  POOL.Pool SQL.Connection ->
  (News.Handle IO, DataTypes.Id DataTypes.Image) ->
  IO (Either ErrorTypes.GetImageError B.ByteString)
oneImage pool (h, id') = do
  let reqResult = EXS.catch (oneImageExcept pool (h, id')) (ErrorTypes.handleGetImageError h)
  EX.runExceptT reqResult

oneImageExcept ::
  POOL.Pool SQL.Connection ->
  (News.Handle IO, DataTypes.Id DataTypes.Image) ->
  EX.ExceptT ErrorTypes.GetImageError IO B.ByteString
oneImageExcept pool (h, id') = do
  liftIO $ Logger.logInfo (News.hLogHandle h) $ "Request: Get One Image with id " .< id'
  _ <- checkId pool h id'
  res <-
    liftIO
      ( EXS.try
          ( POOL.withResource pool $ \conn ->
              SQL.query
                conn
                [sql|SELECT image_content FROM image WHERE image_id = ?|]
                (SQL.Only id')
          ) ::
          IO (Either EXS.SomeException [SQL.Only String])
      )
  case res of
    Left err -> EXS.throwM $ ErrorTypes.GetImageSomeException err
    Right [SQL.Only content] -> do
      liftIO $ Logger.logInfo (News.hLogHandle h) "oneImageExcept: OK!"
      let rez = R.readMaybe content
      case rez of
        Nothing -> EXS.throwM $ ErrorTypes.GetImageSQLRequestError $ ErrorTypes.SQLRequestError " Did not read image_content from table as ByteString"
        Just contents -> return contents
    Right _ -> EXS.throwM $ ErrorTypes.GetImageSQLRequestError $ ErrorTypes.SQLRequestError " Developer error! "

checkId ::
  POOL.Pool SQL.Connection ->
  News.Handle IO ->
  DataTypes.Id DataTypes.Image ->
  EX.ExceptT ErrorTypes.GetImageError IO (DataTypes.Id DataTypes.Image)
checkId pool h' id' = do
  res <-
    liftIO
      ( EXS.try
          ( POOL.withResource pool $ \conn ->
              SQL.query
                conn
                [sql|SELECT EXISTS (SELECT image_id  FROM image WHERE image_id = ?)|]
                (SQL.Only id')
          ) ::
          IO (Either EXS.SomeException [SQL.Only Bool])
      )
  case res of
    Left err -> EXS.throwM $ ErrorTypes.GetImageSomeException err
    Right [SQL.Only True] -> do
      liftIO $ Logger.logDebug (News.hLogHandle h') "checkId: OK!  Image exist "
      return id'
    Right [SQL.Only False] -> EXS.throwM $ ErrorTypes.InvalidImagedId $ ErrorTypes.InvalidId " Image not exists "
    Right _ -> EXS.throwM $ ErrorTypes.GetImageSQLRequestError $ ErrorTypes.SQLRequestError " Developer error! "
