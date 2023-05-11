module EndPoints.AddOneNews
  ( addNews,
    addOneNews,
  )
where

import qualified Control.Exception.Safe as EXS
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (MonadTrans (lift))
import qualified Control.Monad.Trans.Except as EX
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Pool as POOL
import qualified Database.PostgreSQL.Simple as SQL
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified Database.PostgreSQL.Simple.Types as SQLTypes
import qualified EndPoints.Lib.Category.CategoryIO as CategoryIO
import qualified EndPoints.Lib.Lib as Lib
import qualified EndPoints.Lib.LibIO as LibIO
import qualified EndPoints.Lib.News.NewsIO as NewsIO
import qualified EndPoints.Lib.ThrowError as Throw
import qualified EndPoints.Lib.ToHttpResponse as ToHttpResponse
import qualified EndPoints.Lib.ToText as ToText
import Logger (logDebug, logInfo)
import qualified News
import Servant (Handler)
import qualified System.Directory as SD
import qualified Types.DataTypes as DataTypes
import qualified Types.ErrorTypes as ErrorTypes

addOneNews ::
  News.Handle IO ->
  DataTypes.Db ->
  DataTypes.Token ->
  DataTypes.CreateNewsRequest ->
  Handler DataTypes.News
addOneNews h DataTypes.Db {..} user createNewsReq =
  (>>=)
    (liftIO $ dbAddNews (h, user, createNewsReq))
    ToHttpResponse.toHttpResponse

addNews ::
  POOL.Pool SQL.Connection ->
  (News.Handle IO, DataTypes.Token, DataTypes.CreateNewsRequest) ->
  IO (Either ErrorTypes.AddEditNewsError DataTypes.News)
addNews pool (h, token, r) = EX.runExceptT $ addNewsExcept pool (h, token, r)

addNewsExcept ::
  POOL.Pool SQL.Connection ->
  (News.Handle IO, DataTypes.Token, DataTypes.CreateNewsRequest) ->
  EX.ExceptT ErrorTypes.AddEditNewsError IO DataTypes.News
addNewsExcept pool (h, token, r@DataTypes.CreateNewsRequest {..}) = do
  user <- EX.withExceptT ErrorTypes.AddEditNewsSearchUserError (LibIO.searchUser pool h token)
  liftIO $ Logger.logInfo (News.hLogHandle h) $ "\n\nRequest: Add One News: \n" <> ToText.toText r <> "by user: " <> ToText.toText user
  _ <- EX.withExceptT ErrorTypes.InvalidPermissionAddEditNews (Lib.checkUserAuthor h user)
  _ <- checkImageFilesExist h r
  _ <- checkPngImages h r
  _ <- checkBase64Images h r
  _ <- EX.withExceptT ErrorTypes.InvalidCategoryIdAddEditNews (CategoryIO.checkCategoryExistsById pool h newsCategoryId)
  categories' <- EX.withExceptT ErrorTypes.InvalidCategoryIdAddEditNews (CategoryIO.getCategoriesById pool h newsCategoryId)
  images' <- addAllImages pool h r
  addNewsToDB pool h user categories' r images'

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
      liftIO $ Logger.logDebug (News.hLogHandle h) " checkImageFilesExist: OK!"
      return r
    else Throw.throwNotExists h "checkImageFilesExist" $ ErrorTypes.InvalidContent " No such file or directory "

checkPngImages ::
  EXS.MonadThrow m =>
  Monad m =>
  News.Handle m ->
  DataTypes.CreateNewsRequest ->
  EX.ExceptT ErrorTypes.AddEditNewsError m DataTypes.CreateNewsRequest
checkPngImages _ r@(DataTypes.CreateNewsRequest _ _ _ Nothing _) =
  return r
checkPngImages h r@(DataTypes.CreateNewsRequest _ _ _ (Just req) _) =
  if length (filter (\x -> DataTypes.format x == "png") req) == length req
    then do
      lift $ Logger.logDebug (News.hLogHandle h) " checkPngImages: OK!"
      return r
    else Throw.throwNotPng h "checkPngImages" $ ErrorTypes.InvalidContent " Image not png "

checkBase64Images ::
  News.Handle IO ->
  DataTypes.CreateNewsRequest ->
  EX.ExceptT ErrorTypes.AddEditNewsError IO DataTypes.CreateNewsRequest
checkBase64Images _ r@(DataTypes.CreateNewsRequest _ _ _ Nothing _) =
  return r
checkBase64Images h r@(DataTypes.CreateNewsRequest _ _ _ (Just req) _) = do
  imageFiles <- EX.withExceptT ErrorTypes.ImageFileAddEditNewsNotExists (mapM (NewsIO.tryReadImageFile . DataTypes.image) req)
  if length (filter Base64.isBase64 imageFiles) == length req
    then do
      liftIO $ Logger.logDebug (News.hLogHandle h) " checkBase64Image: OK!"
      return r
    else Throw.throwNotBase64 h "checkBase64Images" $ ErrorTypes.InvalidContent " Image not Base64 "

-- | addAllImagesIO Add all the pictures for the news.
addAllImages ::
  POOL.Pool SQL.Connection ->
  News.Handle IO ->
  DataTypes.CreateNewsRequest ->
  EX.ExceptT ErrorTypes.AddEditNewsError IO [DataTypes.Id DataTypes.Image]
addAllImages _ _ (DataTypes.CreateNewsRequest _ _ _ Nothing _) = return []
addAllImages pool h (DataTypes.CreateNewsRequest _ _ _ (Just req) _) = do
  rez <- mapM (NewsIO.addImageNews pool h) req
  liftIO $ Logger.logDebug (News.hLogHandle h) " addAllImages: OK!"
  return rez

addNewsToDB ::
  POOL.Pool SQL.Connection ->
  News.Handle IO ->
  DataTypes.User ->
  [DataTypes.Category] ->
  DataTypes.CreateNewsRequest ->
  [DataTypes.Id DataTypes.Image] ->
  EX.ExceptT ErrorTypes.AddEditNewsError IO DataTypes.News
addNewsToDB pool h DataTypes.User {..} categories DataTypes.CreateNewsRequest {..} idImages = do
  let imageUris = Lib.imagesURIs h idImages
  created <- liftIO Lib.currentDay
  res <-
    liftIO
      ( EXS.try
          ( POOL.withResource pool $ \conn ->
              SQL.query
                conn
                [sql| INSERT INTO news (news_images_id,  news_title , news_created, news_author_login, news_category_id, news_text,  news_published ) VALUES (?, ?, ?, ?, ?, ?,? ) RETURNING news_id |]
                ( SQLTypes.PGArray idImages,
                  title,
                  show created,
                  userLogin,
                  newsCategoryId,
                  text,
                  published
                )
          ) ::
          IO (Either EXS.SomeException [SQL.Only (DataTypes.Id DataTypes.News)])
      )
  case res of
    Left err -> Throw.throwSomeException h "addNewsToDB  " err
    Right [SQL.Only idNews] -> do
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
      liftIO $ Logger.logInfo (News.hLogHandle h) $ "addNewsToDB: OK!" <> ToText.toText news
      return news
    Right _ -> Throw.throwSqlRequestError h "addNewsToDB " (ErrorTypes.SQLRequestError "Developer error!")
