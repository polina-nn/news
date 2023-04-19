module EndPoints.Lib.News.NewsIO
  ( addImageNews,
    toNews,
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
import qualified EndPoints.Lib.ThrowRequestError as Throw
import Logger (logDebug, (.<))
import qualified News
import qualified Types.DataTypes as DataTypes
import qualified Types.ErrorTypes as ErrorTypes

-- | addImageNews adding one pictures to the table of pictures when working with news
addImageNews ::
  POOL.Pool SQL.Connection ->
  News.Handle IO ->
  DataTypes.CreateImageRequest ->
  EX.ExceptT ErrorTypes.AddEditNewsError IO (DataTypes.Id DataTypes.ImageId)
addImageNews pool h DataTypes.CreateImageRequest {..} = do
  content <- liftIO $ B.readFile image
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
          IO (Either EXS.SomeException [SQL.Only Int])
      )
  case res of
    Left err -> Throw.throwSqlRequestError h ("addImageNews", show err)
    Right [SQL.Only idIm] -> do
      liftIO $ Logger.logDebug (News.hLogHandle h) ("addImageNews: OK! Image_id  " .< show idIm)
      return $ DataTypes.Id {getId = idIm}
    Right _ -> Throw.throwSqlRequestError h ("addImageNews", "Developer error!")

toNews ::
  POOL.Pool SQL.Connection ->
  News.Handle IO ->
  NewsHelpTypes.DbNews ->
  EX.ExceptT ErrorTypes.GetNewsError IO DataTypes.News
toNews con h NewsHelpTypes.DbNews {..} = do
  categories <- CategoryIO.getCategoriesById con h dbNewsCategoryId
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
