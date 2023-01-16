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
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as Base64
import qualified Database.PostgreSQL.Simple as SQL
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified EndPoints.Lib.Lib as Lib
import qualified EndPoints.Lib.ToHttpResponse as ToHttpResponse
import Logger (logDebug, logError, (.<))
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
    (liftIO $ _addImage (h, user, createImageReq))
    ToHttpResponse.toHttpResponse

addImage ::
  SQL.Connection ->
  (News.Handle IO, DataTypes.User, DataTypes.CreateImageRequest) ->
  IO (Either ErrorTypes.AddImageError DataTypes.URI)
addImage conn (h, user, createImage) = do
  Logger.logDebug (News.hLogHandle h) "Request: Add One Image"
  allCheck <-
    Lib.checkUserAuthor h user >>= checkImageFileExist h createImage
      >>= checkPngImage h
      >>= checkBase64Image h
  case allCheck :: Either ErrorTypes.AddImageError ImageDecodeBase64ByteString of
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

checkImageFileExist ::
  News.Handle IO ->
  DataTypes.CreateImageRequest ->
  Either ErrorTypes.InvalidAuthorPermission m ->
  IO (Either ErrorTypes.AddImageError DataTypes.CreateImageRequest)
checkImageFileExist _ _ (Left err) =
  return $ Left $ ErrorTypes.InvalidPermissionAddImage err
checkImageFileExist h r@DataTypes.CreateImageRequest {..} (Right _) = do
  rez <- SD.doesFileExist image
  if rez
    then do
      Logger.logDebug (News.hLogHandle h) "checkImageFileExist: OK!"
      return $ Right r
    else do
      Logger.logError
        (News.hLogHandle h)
        ( "ERROR "
            .< ErrorTypes.NotExistImageFile
              ( ErrorTypes.InvalidContent
                  ( "checkImageFileExist: BAD!  File: does not exist (No such file or directory) "
                      ++ image
                  )
              )
        )
      return . Left $
        ErrorTypes.NotExistImageFile $ ErrorTypes.InvalidContent []

checkPngImage ::
  News.Handle IO ->
  Either ErrorTypes.AddImageError DataTypes.CreateImageRequest ->
  IO (Either ErrorTypes.AddImageError DataTypes.CreateImageRequest)
checkPngImage _ (Left err) = return $ Left err
checkPngImage h (Right r@DataTypes.CreateImageRequest {..}) =
  if format == "png"
    then do
      Logger.logDebug (News.hLogHandle h) "checkPngImage: OK!"
      return $ Right r
    else do
      Logger.logError
        (News.hLogHandle h)
        ( "ERROR "
            .< ErrorTypes.NotPngImage
              ( ErrorTypes.InvalidContent ("checkPngImage: BAD! Format " ++ format)
              )
        )
      return . Left $ ErrorTypes.NotPngImage $ ErrorTypes.InvalidContent []

checkBase64Image ::
  News.Handle IO ->
  Either ErrorTypes.AddImageError DataTypes.CreateImageRequest ->
  IO (Either ErrorTypes.AddImageError ImageDecodeBase64ByteString)
checkBase64Image _ (Left err) = return $ Left err
checkBase64Image h (Right DataTypes.CreateImageRequest {..}) = do
  imageFile <- B.readFile image
  case Base64.decodeBase64 imageFile of
    Right val -> do
      Logger.logDebug (News.hLogHandle h) "checkBase64Image: OK!"
      return $ Right val
    Left err -> do
      Logger.logError (News.hLogHandle h) ("ERROR " .< ErrorTypes.NotBase64Image (ErrorTypes.InvalidContent ("checkBase64Image: BAD!" ++ show err)))
      return . Left $ ErrorTypes.NotBase64Image $ ErrorTypes.InvalidContent []

addImageIO ::
  SQL.Connection ->
  News.Handle IO ->
  DataTypes.CreateImageRequest ->
  ImageDecodeBase64ByteString ->
  IO (Either ErrorTypes.AddImageError DataTypes.URI)
addImageIO conn h DataTypes.CreateImageRequest {..} im = do
  resId <-
    SQL.query_ conn [sql| select NEXTVAL('image_id_seq');|] :: IO [SQL.Only Int]
  case resId of
    [val] -> do
      let idIm = SQL.fromOnly val
      res <-
        SQL.execute
          conn
          "INSERT INTO image (image_id, image_name, image_type, image_content) VALUES (?,?,?,?)"
          (idIm, file, format, show im)
      case res of
        1 -> do
          let uriBegin = show $ News.hURIConfig h
              uriEnd = show $ DataTypes.URI' {uriPath = "image", uriId = idIm}
          Logger.logDebug (News.hLogHandle h) ("addImageIO: OK! " .< (uriBegin ++ uriEnd))
          return $ Right $ show uriBegin ++ show uriEnd
        _ -> do
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
    _ -> do
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
