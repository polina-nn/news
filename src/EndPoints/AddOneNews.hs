module EndPoints.AddOneNews
  ( addNews,
    addOneNews,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (MonadTrans (lift))
import qualified Control.Monad.Trans.Except as EX
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as SQL
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified Database.PostgreSQL.Simple.Types as SQLTypes
import qualified EndPoints.Lib.Category.Category as Category
import qualified EndPoints.Lib.Lib as Lib
import qualified EndPoints.Lib.News.NewsIO as NewsIO
import qualified EndPoints.Lib.ToHttpResponse as ToHttpResponse
import qualified EndPoints.Lib.ToText as ToText
import Logger (logDebug, logError, logInfo, (.<))
import qualified News
import Servant (Handler)
import qualified System.Directory as SD
import qualified Types.DataTypes as DataTypes
import qualified Types.ErrorTypes as ErrorTypes

type IdNews = Int

type IdImage = Int

addOneNews ::
  News.Handle IO ->
  DataTypes.Db ->
  DataTypes.User ->
  DataTypes.CreateNewsRequest ->
  Handler DataTypes.News
addOneNews h DataTypes.Db {..} user createNewsReq =
  (>>=)
    (liftIO $ dbAddNews (h, user, createNewsReq))
    ToHttpResponse.toHttpResponse

addNews ::
  SQL.Connection ->
  (News.Handle IO, DataTypes.User, DataTypes.CreateNewsRequest) ->
  IO (Either ErrorTypes.AddEditNewsError DataTypes.News)
addNews conn (h, user, r) = EX.runExceptT $ addNewsExcept conn (h, user, r)

addNewsExcept ::
  SQL.Connection ->
  (News.Handle IO, DataTypes.User, DataTypes.CreateNewsRequest) ->
  EX.ExceptT ErrorTypes.AddEditNewsError IO DataTypes.News
addNewsExcept conn (h, user, r) = do
  liftIO $ Logger.logInfo (News.hLogHandle h) $ T.concat ["Request: Add One News: \n", ToText.toText r, "by user: ", ToText.toText user]
  _ <- EX.withExceptT ErrorTypes.InvalidPermissionAddEditNews (Lib.checkUserAuthor h user)
  categories' <- checkImageFilesExist h r >> checkPngImages h r >> checkBase64Images h r >> checkCategoryId conn h r
  newsId' <- getNewsId conn h
  images' <- addAllImages conn h r
  addNewsToDB conn h user categories' r newsId' images'

checkImageFilesExist ::
  News.Handle IO ->
  DataTypes.CreateNewsRequest ->
  EX.ExceptT ErrorTypes.AddEditNewsError IO DataTypes.CreateNewsRequest
checkImageFilesExist _ r@(DataTypes.CreateNewsRequest _ _ _ Nothing _) =
  return r
checkImageFilesExist h r@(DataTypes.CreateNewsRequest _ _ _ (Just req) _) = do
  rez <- liftIO $ mapM (SD.doesFileExist . DataTypes.image) req
  if and rez
    then do
      liftIO $ Logger.logDebug (News.hLogHandle h) "checkImageFilesExist: OK!"
      return r
    else do
      liftIO $
        Logger.logError
          (News.hLogHandle h)
          ( "ERROR "
              .< ErrorTypes.NotExistImageFileAddEditNews
                ( ErrorTypes.InvalidContent
                    "checkImageFileExist: BAD!  File: does not exist (No such file or directory) "
                )
          )
      EX.throwE $
        ErrorTypes.NotExistImageFileAddEditNews $ ErrorTypes.InvalidContent []

checkPngImages ::
  Monad m =>
  News.Handle m ->
  DataTypes.CreateNewsRequest ->
  EX.ExceptT ErrorTypes.AddEditNewsError m DataTypes.CreateNewsRequest
checkPngImages _ r@(DataTypes.CreateNewsRequest _ _ _ Nothing _) =
  return r
checkPngImages h r@(DataTypes.CreateNewsRequest _ _ _ (Just req) _) =
  if length (filter (\x -> DataTypes.format x == "png") req) == length req
    then do
      lift $ Logger.logDebug (News.hLogHandle h) "checkPngImages: OK!"
      return r
    else do
      lift $
        Logger.logError
          (News.hLogHandle h)
          ( "ERROR "
              .< ErrorTypes.NotPngImageAddEditNews
                ( ErrorTypes.InvalidContent "checkPngImages: BAD!"
                )
          )
      EX.throwE $
        ErrorTypes.NotPngImageAddEditNews $ ErrorTypes.InvalidContent []

checkBase64Images ::
  News.Handle IO ->
  DataTypes.CreateNewsRequest ->
  EX.ExceptT ErrorTypes.AddEditNewsError IO DataTypes.CreateNewsRequest
checkBase64Images _ r@(DataTypes.CreateNewsRequest _ _ _ Nothing _) =
  return r
checkBase64Images h r@(DataTypes.CreateNewsRequest _ _ _ (Just req) _) = do
  imageFiles <- liftIO $ mapM (B.readFile . DataTypes.image) req
  if length (filter Base64.isBase64 imageFiles) == length req
    then do
      liftIO $ Logger.logDebug (News.hLogHandle h) "checkBase64Image: OK!"
      return r
    else do
      liftIO $
        Logger.logError
          (News.hLogHandle h)
          ( "ERROR "
              .< ErrorTypes.NotBase64ImageAddEditNews
                ( ErrorTypes.InvalidContent "checkBase64Image: BAD!"
                )
          )
      EX.throwE $
        ErrorTypes.NotBase64ImageAddEditNews $ ErrorTypes.InvalidContent []

-- | checkCategory -- check if category with the id (from request) exists and get a list of categories from the root to this category
checkCategoryId ::
  SQL.Connection ->
  News.Handle IO ->
  DataTypes.CreateNewsRequest ->
  EX.ExceptT ErrorTypes.AddEditNewsError IO [DataTypes.Category]
checkCategoryId conn h DataTypes.CreateNewsRequest {..} = do
  res <-
    liftIO
      ( SQL.query
          conn
          [sql| SELECT category_path  FROM category WHERE category_id = ?|]
          (SQL.Only newsCategoryId) ::
          IO [SQL.Only String]
      )
  case res of
    [val] -> do
      let path = SQL.fromOnly val
      result <-
        liftIO
          ( SQL.query
              conn
              [sql| SELECT category_path, category_id, category_name FROM category WHERE ? LIKE category_path||'%' ORDER BY category_path |]
              (SQL.Only path)
          )
      let categories = Prelude.map Category.toCategories result
      return categories
    _ -> do
      liftIO $
        Logger.logError
          (News.hLogHandle h)
          ( "ERROR "
              .< ErrorTypes.InvalidCategoryIdAddEditNews
                ( ErrorTypes.InvalidContent
                    "checkCategoryId: BAD! Category not exists"
                )
          )
      EX.throwE $ ErrorTypes.InvalidCategoryIdAddEditNews $ ErrorTypes.InvalidContent []

-- getNewsIdIO  get id for new news
getNewsId ::
  SQL.Connection ->
  News.Handle IO ->
  EX.ExceptT ErrorTypes.AddEditNewsError IO IdNews
getNewsId conn h = do
  resId <-
    liftIO
      ( SQL.query_
          conn
          [sql| select NEXTVAL('news_id_seq')|]
      )
  case resId of
    [val] -> do
      let idNews = SQL.fromOnly val
      liftIO $ Logger.logDebug (News.hLogHandle h) ("getNewsId: OK! News_id is " .< idNews)
      return idNews
    _ -> do
      liftIO $
        Logger.logError (News.hLogHandle h) $
          T.pack $
            show $
              ErrorTypes.AddEditNewsSQLRequestError $
                ErrorTypes.SQLRequestError "getNewsId: BAD!"
      EX.throwE $
        ErrorTypes.AddEditNewsSQLRequestError $ ErrorTypes.SQLRequestError []

-- | addAllImagesIO Add all the pictures for the news.
addAllImages ::
  SQL.Connection ->
  News.Handle IO ->
  DataTypes.CreateNewsRequest ->
  EX.ExceptT ErrorTypes.AddEditNewsError IO [IdImage]
addAllImages _ _ (DataTypes.CreateNewsRequest _ _ _ Nothing _) = return []
addAllImages conn h (DataTypes.CreateNewsRequest _ _ _ (Just req) _) = do
  rez <- mapM (NewsIO.addImageNews conn h) req
  liftIO $ Logger.logDebug (News.hLogHandle h) "addAllImages: OK!"
  return rez

addNewsToDB ::
  SQL.Connection ->
  News.Handle IO ->
  DataTypes.User ->
  [DataTypes.Category] ->
  DataTypes.CreateNewsRequest ->
  IdNews ->
  [IdImage] ->
  EX.ExceptT ErrorTypes.AddEditNewsError IO DataTypes.News
addNewsToDB conn h DataTypes.User {..} categories DataTypes.CreateNewsRequest {..} idNews idImages = do
  let imageUris = map (Lib.imageIdToURI h) idImages
  created <- liftIO Lib.currentDay
  res <-
    liftIO
      ( SQL.execute
          conn
          [sql| INSERT INTO news (news_images_id, news_id, news_title , news_created, news_author_login, news_category_id, news_text,  news_published ) VALUES (?, ?, ?, ?, ?, ?,?,? ) |]
          ( SQLTypes.PGArray idImages,
            idNews,
            title,
            show created,
            userLogin,
            newsCategoryId,
            text,
            published
          )
      )
  case read (show res) :: Int of
    1 -> do
      let news =
            DataTypes.News
              { newsTitle = title,
                newsCreated = created,
                newsAuthor = userName,
                newsCategory = categories,
                newsText = text,
                newsImages = imageUris,
                newsPublished = published,
                newsId = idNews
              }
      liftIO $ Logger.logInfo (News.hLogHandle h) $ T.concat ["addNewsToDB: OK!", ToText.toText news]
      return news
    _ -> do
      liftIO $
        Logger.logError
          (News.hLogHandle h)
          ( "ERROR "
              .< ErrorTypes.AddEditNewsSQLRequestError
                ( ErrorTypes.SQLRequestError "addNewsToDB! Don't INSERT INTO  news table"
                )
          )
      EX.throwE $
        ErrorTypes.AddEditNewsSQLRequestError $ ErrorTypes.SQLRequestError []
