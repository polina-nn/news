module EndPoints.Lib.News.NewsIO
  ( addImageNews,
    toNews,
    tryReadImageFile,
  )
where

import qualified Control.Exception.Safe as EXS
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Control.Monad.Trans.Except as EX
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Pool as POOL
import qualified Database.PostgreSQL.Simple as SQL
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified EndPoints.Lib.Category.CategoryIO as CategoryIO
import qualified EndPoints.Lib.Lib as Lib
import qualified EndPoints.Lib.News.NewsHelpTypes as NewsHelpTypes
import Logger (logDebug, (.<))
import qualified News
import qualified Types.DataTypes as DataTypes
import qualified Types.ErrorTypes as ErrorTypes

-- | addImageNews adding one pictures to the table of pictures when working with news
addImageNews ::
  POOL.Pool SQL.Connection ->
  News.Handle IO ->
  DataTypes.CreateImageRequest ->
  EX.ExceptT ErrorTypes.AddEditNewsError IO (DataTypes.Id DataTypes.Image)
addImageNews pool h DataTypes.CreateImageRequest {..} = do
  content <- EX.withExceptT ErrorTypes.NotBase64ImageAddEditNews (tryReadImageFile image)
  let imageDecodeBase64ByteString = Base64.decodeBase64Lenient content
  res <-
    liftIO
      ( EXS.try
          ( POOL.withResource pool $ \conn ->
              SQL.query
                conn
                [sql| INSERT INTO image ( image_name, image_type, image_content) VALUES (?,?,?) RETURNING  image_id  |]
                (file, format, show imageDecodeBase64ByteString)
          ) ::
          IO (Either EXS.SomeException [SQL.Only (DataTypes.Id DataTypes.Image)])
      )
  case res of
    Left err -> EXS.throwM $ ErrorTypes.AddEditNewsSomeException err
    Right [SQL.Only idIm] -> do
      liftIO $ Logger.logDebug (News.hLogHandle h) ("addImageNews: OK! Image_id  " .< show idIm)
      return idIm
    Right _ -> EXS.throwM $ ErrorTypes.AddEditNewsSQLRequestError $ ErrorTypes.SQLRequestError " Developer error"

toNews ::
  POOL.Pool SQL.Connection ->
  News.Handle IO ->
  NewsHelpTypes.DbNews ->
  EX.ExceptT ErrorTypes.GetNewsError IO DataTypes.News
toNews con h NewsHelpTypes.DbNews {..} = do
  categories <- EX.withExceptT ErrorTypes.InvalidCategoryIdGetNews (EXS.catch (CategoryIO.getCategoriesById con h dbNewsCategoryId) (ErrorTypes.handleInvalidContentCategoryId h))
  let news =
        DataTypes.News
          { newsTitle = dbNewsTitle,
            newsCreated = dbNewsCreated,
            newsAuthor = dbNewsAuthor,
            newsCategory = categories,
            newsText = dbNewsText,
            newsImages = Lib.imagesURIs h dbNewsImagesId,
            newsPublished = dbNewsPublished,
            newsId = dbNewsId
          }
  return news

tryReadImageFile :: FilePath -> EX.ExceptT ErrorTypes.InvalidContent IO B.ByteString
tryReadImageFile filePath = do
  imageFile <- liftIO (EXS.try (B.readFile filePath) :: IO (Either EXS.SomeException B.ByteString))
  case imageFile of
    Right val -> return val
    Left err -> EX.throwE $ ErrorTypes.InvalidContent (show err)
