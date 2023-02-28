module EndPoints.Lib.News.NewsIO
  ( addImageNews, -- use in EndPoints.AddOneNews, EndPoints.EditOneNews
    toNews, --use in EndPoints.GetNewsList,  EndPoints.GetAuthorsNewsList
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Control.Monad.Trans.Except as EX
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as Base64
import qualified Database.PostgreSQL.Simple as SQL
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified EndPoints.Lib.Category.Category as Category
import qualified EndPoints.Lib.Lib as Lib
import qualified EndPoints.Lib.News.NewsHelpTypes as NewsHelpTypes
import Logger (logDebug, logError, (.<))
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
      ( SQL.query_
          conn
          [sql| select NEXTVAL('image_id_seq')|]
      )
  case resId of
    [val] -> do
      let idIm = SQL.fromOnly val
          imageDecodeBase64ByteString = Base64.decodeBase64Lenient content
      res <-
        liftIO
          ( SQL.execute
              conn
              [sql| INSERT INTO image (image_id, image_name, image_type, image_content) VALUES (?,?,?,?)|]
              (idIm, file, format, show imageDecodeBase64ByteString)
          )
      case res of
        1 -> do
          liftIO $ Logger.logDebug (News.hLogHandle h) ("addImageNews: OK! Image_id  " .< show idIm)
          return idIm
        _ -> sqlRequestError
    _ -> sqlRequestError
  where
    sqlRequestError :: EX.ExceptT ErrorTypes.AddEditNewsError IO IdImage
    sqlRequestError = do
      liftIO $
        Logger.logError
          (News.hLogHandle h)
          ( "ERROR "
              .< ErrorTypes.AddEditNewsSQLRequestError
                ( ErrorTypes.SQLRequestError "addImageNews: BAD!"
                )
          )
      EX.throwE $ ErrorTypes.AddEditNewsSQLRequestError $ ErrorTypes.SQLRequestError []

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
getCategories con h''' path = do
  res <-
    liftIO $
      SQL.query
        con
        [sql| SELECT category_path, category_id, category_name FROM category 
            WHERE ? LIKE category_path||'%' ORDER BY category_path |]
        (SQL.Only path)
  case res of
    [] -> do
      liftIO $
        Logger.logError
          (News.hLogHandle h''')
          ( "ERROR "
              .< ErrorTypes.GetNewsSQLRequestError
                ( ErrorTypes.SQLRequestError "getNewsCategoryIO: getCategories : BAD "
                )
          )
      EX.throwE $ ErrorTypes.GetNewsSQLRequestError $ ErrorTypes.SQLRequestError []
    _ -> do
      let categories = Prelude.map Category.toCategories res
      return categories
