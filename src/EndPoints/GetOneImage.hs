module EndPoints.GetOneImage
  ( getOneImage,
    oneImage,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Control.Monad.Trans.Except as EX
import qualified Data.ByteString as B
import qualified Database.PostgreSQL.Simple as SQL
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified DbConnect
import qualified EndPoints.Lib.ToHttpResponse as ToHttpResponse
import Logger (logDebug, logError, logInfo, (.<))
import qualified News
import Servant (Handler)
import qualified Types.DataTypes as DataTypes
import qualified Types.ErrorTypes as ErrorTypes

getOneImage :: News.Handle IO -> DataTypes.Db -> Integer -> Handler B.ByteString
getOneImage h DataTypes.Db {..} idImage =
  (>>=) (liftIO $ dbOneImage (h, idImage)) ToHttpResponse.toHttpResponse

oneImage ::
  SQL.Connection ->
  (News.Handle IO, Integer) ->
  IO (Either ErrorTypes.GetImageError B.ByteString)
oneImage _ (h, id') = EX.runExceptT $ oneImageExcept (h, id')

oneImageExcept ::
  (News.Handle IO, Integer) ->
  EX.ExceptT ErrorTypes.GetImageError IO B.ByteString
oneImageExcept (h, id') = do
  liftIO $ Logger.logInfo (News.hLogHandle h) $ "Request: Get One Image with id " .< id'
  conn <- EX.withExceptT ErrorTypes.GetImageSQLRequestError $ DbConnect.tryRequestConnectDb h
  _ <- checkId conn (h, id')
  res <-
    liftIO
      ( SQL.query
          conn
          [sql|SELECT image_content FROM image WHERE image_id = ?|]
          (SQL.Only id')
      )
  let rez = SQL.fromOnly . head $ res
  liftIO $
    Logger.logInfo
      (News.hLogHandle h)
      "oneImage: OK!"
  liftIO $ SQL.close conn
  return $ read rez

checkId ::
  SQL.Connection ->
  (News.Handle IO, Integer) ->
  EX.ExceptT ErrorTypes.GetImageError IO Integer
checkId conn (h', id') = do
  res <-
    liftIO $
      SQL.query
        conn
        [sql|SELECT EXISTS (SELECT image_id  FROM image WHERE image_id = ?)|]
        (SQL.Only id')
  if SQL.fromOnly $ head res
    then do
      liftIO $ Logger.logDebug (News.hLogHandle h') "checkId: OK!  Image exist "
      return id'
    else do
      liftIO $ Logger.logError (News.hLogHandle h') ("ERROR " .< ErrorTypes.InvalidImagedId (ErrorTypes.InvalidId "checkId: BAD! Image not exist"))
      EX.throwE $ ErrorTypes.InvalidImagedId $ ErrorTypes.InvalidId []
