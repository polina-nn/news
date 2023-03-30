module EndPoints.Lib.News.NewsIO
  ( addImageNews, -- use in EndPoints.AddOneNews, EndPoints.EditOneNews
    toNews, --use in EndPoints.GetNewsList,  EndPoints.GetAuthorsNewsList
  )
where

import qualified Control.Exception.Safe as EXS
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Control.Monad.Trans.Except as EX
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Int as I
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as SQL
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified EndPoints.Lib.Category.Category as Category
import qualified EndPoints.Lib.Lib as Lib
import qualified EndPoints.Lib.News.NewsHelpTypes as NewsHelpTypes
import qualified EndPoints.Lib.ThrowSqlRequestError as Throw
import Logger (logDebug, (.<))
import qualified News
import qualified Types.DataTypes as DataTypes
import qualified Types.ErrorTypes as ErrorTypes

type IdImage = Int

type CategoryPath = String

-- | addImageNews adding one pictures to the table of pictures when working with news
addImageNews ::
  SQL.Connection ->
  News.Handle IO ->
  DataTypes.CreateImageRequest ->
  EX.ExceptT ErrorTypes.AddEditNewsError IO IdImage
addImageNews conn h DataTypes.CreateImageRequest {..} = do
  content <- liftIO $ B.readFile image
  resId <-
    liftIO
      ( EXS.try
          ( SQL.query_
              conn
              [sql| select NEXTVAL('image_id_seq')|]
          ) ::
          IO (Either SQL.SqlError [SQL.Only IdImage])
      )
  case resId of
    Left err -> Throw.throwSqlRequestError h ("addImageNews", show err)
    Right [SQL.Only idIm] -> do
      let imageDecodeBase64ByteString = Base64.decodeBase64Lenient content
      res <-
        liftIO
          ( EXS.try
              ( SQL.execute
                  conn
                  [sql| INSERT INTO image (image_id, image_name, image_type, image_content) VALUES (?,?,?,?)|]
                  (idIm, file, format, show imageDecodeBase64ByteString)
              ) ::
              IO (Either SQL.SqlError I.Int64)
          )
      case res of
        Left err -> Throw.throwSqlRequestError h ("addImageNews", show err)
        Right 1 -> do
          liftIO $ Logger.logDebug (News.hLogHandle h) ("addImageNews: OK! Image_id  " .< show idIm)
          return idIm
        Right _ -> Throw.throwSqlRequestError h ("addImageNews", "Developer error!")
    Right _ -> Throw.throwSqlRequestError h ("addImageNews", "Developer error!")

toNews ::
  SQL.Connection ->
  News.Handle IO ->
  NewsHelpTypes.DbNews ->
  EX.ExceptT ErrorTypes.GetNewsError IO DataTypes.News
toNews con h NewsHelpTypes.DbNews {..} = do
  categories <- getCategories con h dbNewsCategoryPath
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

getCategories ::
  SQL.Connection ->
  News.Handle IO ->
  CategoryPath ->
  EX.ExceptT ErrorTypes.GetNewsError IO [DataTypes.Category]
getCategories con h' path = do
  res <-
    liftIO
      ( EXS.try $
          SQL.query
            con
            [sql| SELECT category_path, category_id, category_name FROM category 
            WHERE ? LIKE category_path||'%' ORDER BY category_path |]
            (SQL.Only path) ::
          IO (Either SQL.SqlError [(String, IdImage, T.Text)])
      )
  case res of
    Left err -> Throw.throwSqlRequestError h' ("getCategories", show err)
    Right [] -> Throw.throwSqlRequestError h' ("getCategories", "Developer error")
    Right val -> do
      let categories = Prelude.map Category.toCategories val
      return categories
