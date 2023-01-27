module EndPoints.GetOneImage
  ( getOneImage,
    oneImage,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.ByteString as B
import qualified Database.PostgreSQL.Simple as SQL
import Database.PostgreSQL.Simple.SqlQQ (sql)
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
oneImage conn (h, id') = do
  Logger.logInfo (News.hLogHandle h) $ "Request: Get One Image with id " .< id'
  resCheckId <- checkId conn (h, id')
  case resCheckId of
    Right _ -> do
      res <-
        SQL.query
          conn
          [sql|SELECT image_content FROM image WHERE image_id = ?|]
          (SQL.Only id')
      let rez = SQL.fromOnly . head $ res
      Logger.logInfo
        (News.hLogHandle h)
        "oneImage: OK!"
      return $ Right $ read rez
    Left err -> return $ Left err

checkId ::
  SQL.Connection ->
  (News.Handle IO, Integer) ->
  IO (Either ErrorTypes.GetImageError Integer)
checkId conn (h', id') = do
  res <-
    SQL.query
      conn
      [sql|SELECT EXISTS (SELECT image_id  FROM image WHERE image_id = ?)|]
      (SQL.Only id')
  if SQL.fromOnly $ head res
    then do
      Logger.logDebug (News.hLogHandle h') "checkId: OK!  Image exist "
      return $ Right id'
    else do
      Logger.logError (News.hLogHandle h') ("ERROR " .< ErrorTypes.InvalidImagedId (ErrorTypes.InvalidId "checkId: BAD! Image not exist"))
      return $ Left $ ErrorTypes.InvalidImagedId $ ErrorTypes.InvalidId []
