module EndPoints.AddOneImage
  ( addImage,
    addOneImage,
  )
where

import qualified Control.Exception.Safe as EXS
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (MonadTrans (lift))
import qualified Control.Monad.Trans.Except as EX
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Pool as POOL
import qualified Database.PostgreSQL.Simple as SQL
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified EndPoints.Lib.Lib as Lib
import qualified EndPoints.Lib.LibIO as LibIO
import qualified EndPoints.Lib.News.NewsIO as NewsIO
import qualified EndPoints.Lib.ThrowError as Throw
import qualified EndPoints.Lib.ToHttpResponse as ToHttpResponse
import qualified EndPoints.Lib.ToText as ToText
import Logger (logDebug, logInfo, (.<))
import qualified News
import Servant (Handler)
import qualified System.Directory as SD
import qualified Types.DataTypes as DataTypes
import qualified Types.ErrorTypes as ErrorTypes

type ImageDecodeBase64ByteString = B.ByteString

addOneImage ::
  News.Handle IO ->
  DataTypes.Db ->
  DataTypes.Token ->
  DataTypes.CreateImageRequest ->
  Handler DataTypes.URI
addOneImage h DataTypes.Db {..} user createImageReq =
  (>>=)
    (liftIO $ dbAddImage (h, user, createImageReq))
    ToHttpResponse.toHttpResponse

addImage ::
  POOL.Pool SQL.Connection ->
  (News.Handle IO, DataTypes.Token, DataTypes.CreateImageRequest) ->
  IO (Either ErrorTypes.AddImageError DataTypes.URI)
addImage pool (h, token, createImage) = EX.runExceptT $ addImageExcept pool (h, token, createImage)

addImageExcept ::
  POOL.Pool SQL.Connection ->
  (News.Handle IO, DataTypes.Token, DataTypes.CreateImageRequest) ->
  EX.ExceptT ErrorTypes.AddImageError IO DataTypes.URI
addImageExcept pool (h, token, createImage) = do
  user <- EX.withExceptT ErrorTypes.AddImageSearchUserError (LibIO.searchUser pool h token)
  liftIO $ Logger.logInfo (News.hLogHandle h) $ "\n\nRequest: Add One Image " <> ToText.toText createImage <> "\nby user: " <> ToText.toText user
  _ <- EX.withExceptT ErrorTypes.InvalidPermissionAddImage (Lib.checkUserAuthor h user)
  _ <- checkPngImage h createImage
  _ <- checkImageFileExist h createImage
  allCheckAndDecodeBase64ByteString <- checkAndDecodeBase64Image h createImage
  addImageToDB pool h createImage allCheckAndDecodeBase64ByteString

checkImageFileExist ::
  News.Handle IO ->
  DataTypes.CreateImageRequest ->
  EX.ExceptT ErrorTypes.AddImageError IO DataTypes.CreateImageRequest
checkImageFileExist h r@DataTypes.CreateImageRequest {..} = do
  rez <- liftIO $ SD.doesFileExist image
  if rez
    then do
      liftIO $ Logger.logDebug (News.hLogHandle h) "checkImageFileExist: OK!"
      return r
    else Throw.throwNotExists h "checkImageFileExist" $ ErrorTypes.InvalidContent " No such file or directory "

checkPngImage ::
  EXS.MonadThrow m =>
  News.Handle m ->
  DataTypes.CreateImageRequest ->
  EX.ExceptT ErrorTypes.AddImageError m DataTypes.CreateImageRequest
checkPngImage h r@DataTypes.CreateImageRequest {..} =
  if format == "png"
    then do
      lift $ Logger.logDebug (News.hLogHandle h) "checkPngImage: OK!"
      return r
    else do
      Throw.throwNotPng h "checkPngImage" $ ErrorTypes.InvalidContent " Image not png "

checkAndDecodeBase64Image ::
  News.Handle IO ->
  DataTypes.CreateImageRequest ->
  EX.ExceptT ErrorTypes.AddImageError IO ImageDecodeBase64ByteString
checkAndDecodeBase64Image h DataTypes.CreateImageRequest {..} = do
  imageFile <- EX.withExceptT ErrorTypes.NotBase64Image (NewsIO.tryReadImageFile image)
  case Base64.decodeBase64 imageFile of
    Right val -> do
      liftIO $ Logger.logDebug (News.hLogHandle h) "checkAndDecodeBase64Image: OK!"
      return val
    Left err -> Throw.throwNotBase64 h "checkBase64Images" $ ErrorTypes.InvalidContent $ " Image not Base64 " <> show err

addImageToDB ::
  POOL.Pool SQL.Connection ->
  News.Handle IO ->
  DataTypes.CreateImageRequest ->
  ImageDecodeBase64ByteString ->
  EX.ExceptT ErrorTypes.AddImageError IO DataTypes.URI
addImageToDB pool h DataTypes.CreateImageRequest {..} im = do
  res <-
    liftIO
      ( EXS.try
          ( POOL.withResource pool $ \conn ->
              SQL.query
                conn
                [sql| INSERT INTO image ( image_name, image_type, image_content) VALUES (?,?,?) RETURNING  image_id |]
                (file, format, show im)
          ) ::
          IO (Either EXS.SomeException [SQL.Only Int])
      )
  case res of
    Left err -> Throw.throwSomeException h "addImageToDB " err
    Right [SQL.Only idIm] -> do
      let uriBegin = show $ News.hURIConfig h
          uriEnd = show $ DataTypes.URI' {uriPath = "image", uriId = idIm}
      liftIO $ Logger.logDebug (News.hLogHandle h) ("addImageToDB: OK! " .< (uriBegin ++ uriEnd))
      return $ uriBegin ++ uriEnd
    Right _ -> Throw.throwSqlRequestError h "addImageToDB " (ErrorTypes.SQLRequestError "Developer error!")
