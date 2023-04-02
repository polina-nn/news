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
import qualified EndPoints.Lib.ThrowSqlRequestError as Throw
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
  POOL.Pool SQL.Connection ->
  (News.Handle IO, Integer) ->
  IO (Either ErrorTypes.GetImageError B.ByteString)
oneImage pool (h, id') = EX.runExceptT $ oneImageExcept pool (h, id')

oneImageExcept ::
  POOL.Pool SQL.Connection ->
  (News.Handle IO, Integer) ->
  EX.ExceptT ErrorTypes.GetImageError IO B.ByteString
oneImageExcept pool (h, id') = do
  liftIO $ Logger.logInfo (News.hLogHandle h) $ "Request: Get One Image with id " .< id'
  _ <- checkId pool h id'
  res <-
    liftIO
      ( POOL.withResource pool $ \conn ->
          EXS.try
            ( SQL.query
                conn
                [sql|SELECT image_content FROM image WHERE image_id = ?|]
                (SQL.Only id')
            ) ::
            IO (Either EXS.SomeException [SQL.Only String])
      )
  case res of
    Left err -> Throw.throwSqlRequestError h ("oneImageExcept", show err)
    Right [SQL.Only content] -> do
      liftIO $ Logger.logInfo (News.hLogHandle h) "oneImageExcept: OK!"
      return $ read content
    Right _ -> Throw.throwSqlRequestError h ("oneImageExcept", "Developer error!")

checkId ::
  POOL.Pool SQL.Connection ->
  News.Handle IO ->
  Integer ->
  EX.ExceptT ErrorTypes.GetImageError IO Integer
checkId pool h' id' = do
  res <-
    liftIO
      ( POOL.withResource pool $ \conn ->
          EXS.try $
            SQL.query
              conn
              [sql|SELECT EXISTS (SELECT image_id  FROM image WHERE image_id = ?)|]
              (SQL.Only id') ::
            IO (Either EXS.SomeException [SQL.Only Bool])
      )
  case res of
    Left err -> Throw.throwSqlRequestError h' ("checkId", show err)
    Right [SQL.Only True] -> do
      liftIO $ Logger.logDebug (News.hLogHandle h') "checkId: OK!  Image exist "
      return id'
    Right [SQL.Only False] -> do
      liftIO $ Logger.logError (News.hLogHandle h') ("ERROR " .< ErrorTypes.InvalidImagedId (ErrorTypes.InvalidId "checkId: BAD! Image not exist"))
      EX.throwE $ ErrorTypes.InvalidImagedId $ ErrorTypes.InvalidId []
    Right _ -> Throw.throwSqlRequestError h' ("checkId", "Developer error!")
