{-# OPTIONS_GHC -Wno-deprecations #-}

module EndPoints.AddOneImage
  ( addImage,
    addOneImage,
  )
where

import Control.Exception.Base
  ( Exception (displayException),
    SomeException (SomeException),
    catch,
    throwIO,
  )
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (MonadTrans (lift))
import qualified Control.Monad.Trans.Except as EX
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as SQL
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified EndPoints.Lib.Lib as Lib
import qualified EndPoints.Lib.ToHttpResponse as ToHttpResponse
import qualified EndPoints.Lib.ToText as ToText
import Logger (logDebug, logError, logInfo, (.<))
import qualified News
import Servant (Handler)
import qualified System.Directory as SD
import qualified Types.DataTypes as DataTypes
import qualified Types.ErrorTypes as ErrorTypes

type ImageDecodeBase64ByteString = B.ByteString

addOneImage ::
  News.Handle IO ->
  DataTypes.Db ->
  DataTypes.User ->
  DataTypes.CreateImageRequest ->
  Handler DataTypes.URI
addOneImage h DataTypes.Db {..} user createImageReq =
  (>>=)
    (liftIO $ dbAddImage (h, user, createImageReq))
    ToHttpResponse.toHttpResponse

addImage ::
  SQL.Connection ->
  (News.Handle IO, DataTypes.User, DataTypes.CreateImageRequest) ->
  IO (Either ErrorTypes.AddImageError DataTypes.URI)
addImage conn (h, user, createImage) = do
  Logger.logInfo (News.hLogHandle h) $ T.concat ["Request: Add One Image ", ToText.toText createImage, "\nby user: ", ToText.toText user]
  allCheckAndDecodeBase64ByteString <- EX.runExceptT $ allCheck (h, user, createImage)
  case allCheckAndDecodeBase64ByteString of
    Left err -> return $ Left err
    Right imageDecodeBase64ByteString ->
      catch
        (addImageIO conn h createImage imageDecodeBase64ByteString)
        handleError
  where
    handleError ::
      SomeException -> IO (Either ErrorTypes.AddImageError DataTypes.URI)
    handleError (SomeException e) = do
      Logger.logError (News.hLogHandle h) ("addImage:handleError: throwIO is unknown error" .< displayException e)
      throwIO e

allCheck :: (News.Handle IO, DataTypes.User, DataTypes.CreateImageRequest) -> EX.ExceptT ErrorTypes.AddImageError IO ImageDecodeBase64ByteString
allCheck (h, user, createImage) = do
  let checkUserAuthor = EX.withExceptT ErrorTypes.InvalidPermissionAddImage (Lib.checkUserAuthor' h user)
  checkUserAuthor >> checkImageFileExist h createImage >> checkPngImage h createImage >> checkAndDecodeBase64Image h createImage

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
    else do
      liftIO $
        Logger.logError
          (News.hLogHandle h)
          ( "ERROR "
              .< ErrorTypes.NotExistImageFile
                ( ErrorTypes.InvalidContent
                    "checkImageFileExist: BAD!  File: does not exist (No such file or directory) "
                )
          )
      EX.throwE $ ErrorTypes.NotExistImageFile $ ErrorTypes.InvalidContent []

checkPngImage ::
  Monad m =>
  News.Handle m ->
  DataTypes.CreateImageRequest ->
  EX.ExceptT ErrorTypes.AddImageError m DataTypes.CreateImageRequest
checkPngImage h r@DataTypes.CreateImageRequest {..} =
  if format == "png"
    then do
      lift $ Logger.logDebug (News.hLogHandle h) "checkPngImage: OK!"
      return r
    else do
      lift $
        Logger.logError
          (News.hLogHandle h)
          ( "ERROR "
              .< ErrorTypes.NotPngImage
                ( ErrorTypes.InvalidContent "checkPngImage: BAD! Format "
                )
          )
      EX.throwE $ ErrorTypes.NotPngImage $ ErrorTypes.InvalidContent []

checkAndDecodeBase64Image ::
  News.Handle IO ->
  DataTypes.CreateImageRequest ->
  EX.ExceptT ErrorTypes.AddImageError IO ImageDecodeBase64ByteString
checkAndDecodeBase64Image h DataTypes.CreateImageRequest {..} = do
  imageFile <- liftIO $ B.readFile image
  case Base64.decodeBase64 imageFile of
    Right val -> do
      liftIO $ Logger.logDebug (News.hLogHandle h) "checkBase64Image: OK!"
      return val
    Left err -> do
      liftIO $ Logger.logError (News.hLogHandle h) ("ERROR " .< ErrorTypes.NotBase64Image (ErrorTypes.InvalidContent ("checkBase64Image: BAD!" ++ show err)))
      EX.throwE $ ErrorTypes.NotBase64Image $ ErrorTypes.InvalidContent []

addImageIO ::
  SQL.Connection ->
  News.Handle IO ->
  DataTypes.CreateImageRequest ->
  ImageDecodeBase64ByteString ->
  IO (Either ErrorTypes.AddImageError DataTypes.URI)
addImageIO conn h DataTypes.CreateImageRequest {..} im = do
  resId <-
    SQL.query_
      conn
      [sql| select NEXTVAL('image_id_seq')|]
  case resId of
    [val] -> do
      let idIm = SQL.fromOnly val
      res <-
        SQL.execute
          conn
          [sql| INSERT INTO image (image_id, image_name, image_type, image_content) VALUES (?,?,?,?) |]
          (idIm, file, format, show im)
      case res of
        1 -> do
          let uriBegin = show $ News.hURIConfig h
              uriEnd = show $ DataTypes.URI' {uriPath = "image", uriId = idIm}
          Logger.logDebug (News.hLogHandle h) ("addImageIO: OK! " .< (uriBegin ++ uriEnd))
          return $ Right $ uriBegin ++ uriEnd
        _ -> sqlRequestError
    _ -> do
      sqlRequestError
  where
    sqlRequestError = do
      Logger.logError
        (News.hLogHandle h)
        ( "ERROR "
            .< ErrorTypes.AddImageSQLRequestError
              ( ErrorTypes.SQLRequestError "addImageIO: BAD!"
              )
        )
      return $
        Left $
          ErrorTypes.AddImageSQLRequestError $ ErrorTypes.SQLRequestError []
