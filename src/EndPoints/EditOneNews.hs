module EndPoints.EditOneNews
  ( editOneNews,
    editNews,
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
import qualified Data.Text as T
import qualified Data.Time as TIME
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

type IdCategory = Int

type IdImage = Int

type CategoryPath = String

editOneNews ::
  News.Handle IO ->
  DataTypes.Db ->
  DataTypes.User ->
  IdNews ->
  DataTypes.EditNewsRequest ->
  Handler DataTypes.News
editOneNews h DataTypes.Db {..} user catId r =
  (>>=) (liftIO $ _editNews (h, user, catId, r)) ToHttpResponse.toHttpResponse

editNews ::
  SQL.Connection ->
  (News.Handle IO, DataTypes.User, IdNews, DataTypes.EditNewsRequest) ->
  IO (Either ErrorTypes.AddEditNewsError DataTypes.News)
editNews conn (h, user, newsId, r) = do
  Logger.logInfo (News.hLogHandle h) $ T.concat ["Request: Edit One News \n", ToText.toText r, "with news id ", T.pack $ show newsId, "\nby user: ", ToText.toText user]
  allCheck <-
    checkIdIO conn h newsId >>= checkUserThisNewsAuthorIO conn h user
      >>= checkImageFilesExistIO h r
      >>= checkCategoryIdIO conn h
      >>= checkPngImages h
      >>= checkBase64ImagesIO h
  case allCheck :: Either ErrorTypes.AddEditNewsError DataTypes.EditNewsRequest of
    Left err -> return $ Left err
    Right _ ->
      catch
        ( newTileIO conn h newsId r >>= newCatIdIO conn h newsId
            >>= newTextIO conn h newsId
            >>= newImagesIO conn h newsId
            >>= newPublishIO conn h newsId
            >>= getNewsCategoryIO conn h newsId
            >>= getNewsImagesIO conn h newsId
            >>= getNewsIO conn h user newsId
        )
        handleError
  where
    handleError ::
      SomeException -> IO (Either ErrorTypes.AddEditNewsError DataTypes.News)
    handleError (SomeException e) = do
      let errMsg = displayException e
      Logger.logError
        (News.hLogHandle h)
        ("editNews:handleError:" .< errMsg)
      throwIO e

newTileIO ::
  SQL.Connection ->
  News.Handle IO ->
  IdNews ->
  DataTypes.EditNewsRequest ->
  IO (Either ErrorTypes.AddEditNewsError DataTypes.EditNewsRequest)
newTileIO _ _ _ r@DataTypes.EditNewsRequest {newTitle = Nothing} =
  return $ Right r
newTileIO conn h newsId r@DataTypes.EditNewsRequest {newTitle = Just title} = do
  res <-
    SQL.execute
      conn
      [sql| UPDATE news SET news_title = ? WHERE news_id = ? ;|]
      (title, newsId)
  case read (show res) :: Int of
    1 -> do
      Logger.logDebug (News.hLogHandle h) "newTileIO: OK! UPDATE news "
      return $ Right r
    _ -> do
      Logger.logError (News.hLogHandle h) ("ERROR " .< ErrorTypes.AddEditNewsSQLRequestError (ErrorTypes.SQLRequestError "newTileIO: BAD! Don't UPDATE  news"))
      return $
        Left $
          ErrorTypes.AddEditNewsSQLRequestError $ ErrorTypes.SQLRequestError []

newCatIdIO ::
  SQL.Connection ->
  News.Handle IO ->
  IdNews ->
  Either ErrorTypes.AddEditNewsError DataTypes.EditNewsRequest ->
  IO (Either ErrorTypes.AddEditNewsError DataTypes.EditNewsRequest)
newCatIdIO _ _ _ (Left err) = return (Left err)
newCatIdIO _ _ _ (Right r@DataTypes.EditNewsRequest {newCategoryId = Nothing}) =
  return $ Right r
newCatIdIO conn h newsId (Right r@DataTypes.EditNewsRequest {newCategoryId = Just catId}) = do
  res <-
    SQL.execute
      conn
      [sql| UPDATE news SET news_category_id = ? WHERE news_id = ? ;|]
      (catId, newsId)
  case read (show res) :: Int of
    1 -> do
      Logger.logDebug (News.hLogHandle h) "newCatIdIO: OK! UPDATE news "
      return $ Right r
    _ -> do
      Logger.logError (News.hLogHandle h) ("ERROR " .< ErrorTypes.AddEditNewsSQLRequestError (ErrorTypes.SQLRequestError "newCatIdIO: BAD! Don't UPDATE  news"))
      return $
        Left $
          ErrorTypes.AddEditNewsSQLRequestError $ ErrorTypes.SQLRequestError []

newTextIO ::
  SQL.Connection ->
  News.Handle IO ->
  IdNews ->
  Either ErrorTypes.AddEditNewsError DataTypes.EditNewsRequest ->
  IO (Either ErrorTypes.AddEditNewsError DataTypes.EditNewsRequest)
newTextIO _ _ _ (Left err) = return (Left err)
newTextIO _ _ _ (Right r@DataTypes.EditNewsRequest {newText = Nothing}) =
  return $ Right r
newTextIO conn h newsId (Right r@DataTypes.EditNewsRequest {newText = Just text}) = do
  res <-
    SQL.execute
      conn
      [sql| UPDATE news SET news_text = ? WHERE news_id = ? ;|]
      (text, newsId)
  case read (show res) :: Int of
    1 -> do
      Logger.logDebug (News.hLogHandle h) "newTextIO: OK! UPDATE news "
      return $ Right r
    _ -> do
      Logger.logError (News.hLogHandle h) ("ERROR " .< ErrorTypes.AddEditNewsSQLRequestError (ErrorTypes.SQLRequestError "newTextIO: BAD! Don't UPDATE  news"))
      return $
        Left $
          ErrorTypes.AddEditNewsSQLRequestError $ ErrorTypes.SQLRequestError []

newImagesIO ::
  SQL.Connection ->
  News.Handle IO ->
  IdNews ->
  Either ErrorTypes.AddEditNewsError DataTypes.EditNewsRequest ->
  IO (Either ErrorTypes.AddEditNewsError DataTypes.EditNewsRequest)
newImagesIO _ _ _ (Left err) = return (Left err)
newImagesIO _ _ _ (Right r@DataTypes.EditNewsRequest {newImages = Nothing}) =
  return $ Right r
newImagesIO conn h newsId (Right r@DataTypes.EditNewsRequest {newImages = Just req}) = do
  rez <- mapM (NewsIO.addImageNewsIO conn h) req
  let rez' = map help rez
  if 0 `notElem` rez'
    then do
      Logger.logDebug (News.hLogHandle h) "addImageNewsIO: OK!"
      res <-
        SQL.execute
          conn
          [sql| UPDATE news SET news_images_id = ? WHERE news_id = ? ;|]
          (SQLTypes.PGArray rez', newsId)
      case read (show res) :: Int of
        1 -> do
          Logger.logDebug (News.hLogHandle h) "newImagesIO: OK! UPDATE news "
          return $ Right r
        _ -> do
          Logger.logError (News.hLogHandle h) ("ERROR " .< ErrorTypes.AddEditNewsSQLRequestError (ErrorTypes.SQLRequestError "newImagesIO: BAD! Don't UPDATE  news"))
          return $
            Left $
              ErrorTypes.AddEditNewsSQLRequestError $
                ErrorTypes.SQLRequestError []
    else do
      Logger.logError (News.hLogHandle h) ("ERROR " .< ErrorTypes.AddEditNewsSQLRequestError (ErrorTypes.SQLRequestError "addImageNewsIO:BAD! Don't add all images."))
      return $
        Left $
          ErrorTypes.AddEditNewsSQLRequestError $ ErrorTypes.SQLRequestError []
  where
    help :: Either ErrorTypes.AddEditNewsError IdImage -> IdImage
    help (Left _) = 0
    help (Right val) = val

newPublishIO ::
  SQL.Connection ->
  News.Handle IO ->
  IdNews ->
  Either ErrorTypes.AddEditNewsError DataTypes.EditNewsRequest ->
  IO (Either ErrorTypes.AddEditNewsError DataTypes.EditNewsRequest)
newPublishIO _ _ _ (Left err) = return (Left err)
newPublishIO _ _ _ (Right r@DataTypes.EditNewsRequest {newPublished = Nothing}) =
  return $ Right r
newPublishIO conn h newsId (Right r@DataTypes.EditNewsRequest {newPublished = Just pub}) = do
  res <-
    SQL.execute
      conn
      [sql| UPDATE news SET news_published = ? WHERE news_id = ? ;|]
      (pub, newsId)
  case read (show res) :: Int of
    1 -> do
      Logger.logDebug (News.hLogHandle h) "newPublishIO: OK! UPDATE news "
      return $ Right r
    _ -> do
      Logger.logError (News.hLogHandle h) ("ERROR " .< ErrorTypes.AddEditNewsSQLRequestError (ErrorTypes.SQLRequestError "newPublishIO: BAD! Don't UPDATE  news"))
      return $
        Left $
          ErrorTypes.AddEditNewsSQLRequestError $ ErrorTypes.SQLRequestError []

--
getNewsCategoryIO ::
  SQL.Connection ->
  News.Handle IO ->
  IdNews ->
  Either ErrorTypes.AddEditNewsError DataTypes.EditNewsRequest ->
  IO (Either ErrorTypes.AddEditNewsError [DataTypes.Category])
getNewsCategoryIO _ _ _ (Left err) = return $ Left err
getNewsCategoryIO conn h idNews (Right _) =
  getNewsCategoryIdIO conn h idNews >>= getCategoryPathIO conn h
    >>= getCategoriesIO conn h
  where
    getNewsCategoryIdIO ::
      SQL.Connection ->
      News.Handle IO ->
      IdNews ->
      IO (Either ErrorTypes.AddEditNewsError IdCategory)
    getNewsCategoryIdIO conn' h' idNews' = do
      res <-
        SQL.query
          conn'
          [sql| SELECT news_category_id   FROM news WHERE  news_id = ? |]
          (SQL.Only idNews') ::
          IO [SQL.Only Int]
      case res of
        [val] -> do
          let categoryId = SQL.fromOnly val
          Logger.logDebug (News.hLogHandle h) ("getNewsCategoryIdIO: OK! " .< categoryId)
          return $ Right categoryId
        _ -> do
          Logger.logError (News.hLogHandle h') ("ERROR " .< ErrorTypes.AddEditNewsSQLRequestError (ErrorTypes.SQLRequestError "getNewsCategoryIO: getNewsCategoryIdIO : BAD!"))
          return $
            Left $
              ErrorTypes.AddEditNewsSQLRequestError $
                ErrorTypes.SQLRequestError []
    getCategoryPathIO ::
      SQL.Connection ->
      News.Handle IO ->
      Either ErrorTypes.AddEditNewsError IdCategory ->
      IO (Either ErrorTypes.AddEditNewsError CategoryPath)
    getCategoryPathIO _ _ (Left err) = return $ Left err
    getCategoryPathIO conn'' h'' (Right categoryId) = do
      res <-
        SQL.query
          conn''
          [sql| SELECT category_path  FROM category WHERE category_id = ?|]
          (SQL.Only categoryId) ::
          IO [SQL.Only String]
      case res of
        [val] -> do
          let categoryPath = SQL.fromOnly val
          Logger.logDebug (News.hLogHandle h) ("getCategoryPathIO: OK! " .< categoryPath)
          return $ Right categoryPath
        _ -> do
          Logger.logError (News.hLogHandle h'') ("ERROR " .< ErrorTypes.AddEditNewsSQLRequestError (ErrorTypes.SQLRequestError "getNewsCategoryIO: getCategoryPathIO : BAD "))
          return $
            Left $
              ErrorTypes.AddEditNewsSQLRequestError $
                ErrorTypes.SQLRequestError []
    getCategoriesIO ::
      SQL.Connection ->
      News.Handle IO ->
      Either ErrorTypes.AddEditNewsError CategoryPath ->
      IO (Either ErrorTypes.AddEditNewsError [DataTypes.Category])
    getCategoriesIO _ _ (Left err) = return $ Left err
    getCategoriesIO con h''' (Right path) = do
      res <-
        SQL.query
          con
          [sql| SELECT category_path, category_id, category_name FROM category WHERE ? LIKE category_path||'%' ORDER BY category_path;|]
          (SQL.Only path)
      case res of
        [] -> do
          Logger.logError
            (News.hLogHandle h''')
            ("ERROR " .< ErrorTypes.AddEditNewsSQLRequestError (ErrorTypes.SQLRequestError "getNewsCategoryIO: getCategoriesIO : BAD "))
          return $
            Left $
              ErrorTypes.AddEditNewsSQLRequestError $
                ErrorTypes.SQLRequestError []
        _ -> do
          let categories = Prelude.map Category.toCategories res
          Logger.logDebug (News.hLogHandle h) "getCategoriesIO: OK! "
          return $ Right categories

getNewsImagesIO ::
  SQL.Connection ->
  News.Handle IO ->
  IdNews ->
  Either ErrorTypes.AddEditNewsError [DataTypes.Category] ->
  IO (Either ErrorTypes.AddEditNewsError ([DataTypes.Category], [IdImage]))
getNewsImagesIO _ _ _ (Left err) = return $ Left err
getNewsImagesIO conn h idNews (Right cats) = do
  res <-
    SQL.query
      conn
      [sql| SELECT EXISTS (SELECT news_images_id FROM news WHERE  news_id = ? AND news_images_id IS NOT NULL) |]
      (SQL.Only idNews) ::
      IO [SQL.Only Bool]
  case res of
    [val] ->
      if SQL.fromOnly val
        then
          ( do
              [ids] <-
                SQL.query
                  conn
                  " SELECT news_images_id    FROM news WHERE  news_id = ? "
                  (SQL.Only idNews) ::
                  IO [SQL.Only (SQLTypes.PGArray IdImage)]
              let idImages = SQLTypes.fromPGArray $ SQL.fromOnly ids
              return (Right (cats, idImages))
          )
        else return (Right (cats, []))
    _ -> do
      Logger.logError (News.hLogHandle h) ("ERROR " .< ErrorTypes.AddEditNewsSQLRequestError (ErrorTypes.SQLRequestError "checkIdIO! Don't checkId category"))
      return $
        Left $
          ErrorTypes.AddEditNewsSQLRequestError $ ErrorTypes.SQLRequestError []

getNewsIO ::
  SQL.Connection ->
  News.Handle IO ->
  DataTypes.User ->
  IdNews ->
  Either ErrorTypes.AddEditNewsError ([DataTypes.Category], [IdImage]) ->
  IO (Either ErrorTypes.AddEditNewsError DataTypes.News)
getNewsIO _ _ _ _ (Left err) = return $ Left err
getNewsIO conn h user idNews (Right (categories, imagesIds)) = do
  res <-
    SQL.query
      conn
      [sql| SELECT news_title , news_created, news_text,  news_published 
                FROM news 
                WHERE  news_id = ? |]
      (SQL.Only idNews) ::
      IO [(T.Text, TIME.Day, T.Text, Bool)]
  case res of
    [val] -> do
      let news = toNews val user categories imagesIds
      Logger.logDebug (News.hLogHandle h) $ T.concat ["addNewsIO: OK!", ToText.toText news]
      return $ Right news
    _ -> do
      Logger.logError (News.hLogHandle h) ("ERROR " .< ErrorTypes.AddEditNewsSQLRequestError (ErrorTypes.SQLRequestError "addNewsIO! Don't INSERT INTO  news table"))
      return $
        Left $
          ErrorTypes.AddEditNewsSQLRequestError $ ErrorTypes.SQLRequestError []
  where
    toNews ::
      (T.Text, TIME.Day, T.Text, Bool) ->
      DataTypes.User ->
      [DataTypes.Category] ->
      [IdImage] ->
      DataTypes.News
    toNews (news_title, news_created, news_text, news_published) DataTypes.User {..} cats imagesId =
      DataTypes.News
        { newsTitle = news_title,
          newsCreated = news_created,
          newsAuthor = userName,
          newsCategory = cats,
          newsText = news_text,
          newsImages = imageUris imagesId,
          newsPublished = news_published
        }
      where
        imageUris :: [IdImage] -> [DataTypes.URI]
        imageUris xs
          | null xs = []
          | otherwise = map (Lib.imageIdToURI h) xs

----- All checks ----

-- | checkIdIO - check news Id  (  id = 7 in  http://localhost:8080/news/7 )
checkIdIO ::
  SQL.Connection ->
  News.Handle IO ->
  IdNews ->
  IO (Either ErrorTypes.AddEditNewsError IdNews)
checkIdIO conn h newsId = do
  res <-
    SQL.query
      conn
      [sql| SELECT EXISTS (SELECT news_id  FROM news WHERE news_id = ?) |]
      (SQL.Only newsId) ::
      IO [SQL.Only Bool]
  case res of
    [] -> do
      Logger.logError (News.hLogHandle h) ("ERROR " .< ErrorTypes.AddEditNewsSQLRequestError (ErrorTypes.SQLRequestError "checkIdIO! Don't checkId category"))
      return $
        Left $
          ErrorTypes.AddEditNewsSQLRequestError $ ErrorTypes.SQLRequestError []
    _ ->
      if SQL.fromOnly $ head res
        then
          ( do
              Logger.logDebug (News.hLogHandle h) ("checkIdIO: OK! Exists news with id " .< newsId)
              return $ Right newsId
          )
        else
          ( do
              Logger.logError (News.hLogHandle h) ("ERROR " .< ErrorTypes.InvalidNewsId (ErrorTypes.InvalidId "checkIdIO: BAD! Not exists news with id "))
              return $
                Left $ ErrorTypes.InvalidNewsId $ ErrorTypes.InvalidId []
          )

-- | checkUserThisNewsAuthorIO - Is he the author of this news?
checkUserThisNewsAuthorIO ::
  SQL.Connection ->
  News.Handle IO ->
  DataTypes.User ->
  Either ErrorTypes.AddEditNewsError IdNews ->
  IO (Either ErrorTypes.AddEditNewsError IdNews)
checkUserThisNewsAuthorIO _ _ _ (Left err) = return $ Left err
checkUserThisNewsAuthorIO conn h DataTypes.User {..} (Right newsId) = do
  res <-
    SQL.query
      conn
      [sql| SELECT news_author_login  FROM news WHERE news_id = ? |]
      (SQL.Only newsId) ::
      IO [SQL.Only String]
  case res of
    [newsAuthorLogin] ->
      if SQL.fromOnly newsAuthorLogin == userLogin
        then do
          Logger.logDebug (News.hLogHandle h) "checkUserThisNewsAuthorIO: OK!"
          return $ Right newsId
        else do
          Logger.logError (News.hLogHandle h) ("ERROR " .< ErrorTypes.InvalidPermissionAddEditNews (ErrorTypes.InvalidAuthorPermission "checkUserThisNewsAuthorIO: BAD! User is not this news author. Invalid Permission for this request."))
          return . Left $
            ErrorTypes.InvalidPermissionAddEditNews $
              ErrorTypes.InvalidAuthorPermission []
    _ -> do
      Logger.logError (News.hLogHandle h) ("ERROR " .< ErrorTypes.AddEditNewsSQLRequestError (ErrorTypes.SQLRequestError "checkUserThisNewsAuthorIO: BAD!"))
      return $
        Left $
          ErrorTypes.AddEditNewsSQLRequestError $ ErrorTypes.SQLRequestError []

checkImageFilesExistIO ::
  News.Handle IO ->
  DataTypes.EditNewsRequest ->
  Either ErrorTypes.AddEditNewsError IdNews ->
  IO (Either ErrorTypes.AddEditNewsError DataTypes.EditNewsRequest)
checkImageFilesExistIO _ _ (Left err) = return $ Left err
checkImageFilesExistIO _ r@DataTypes.EditNewsRequest {newImages = Nothing} (Right _) =
  return $ Right r
checkImageFilesExistIO h r@DataTypes.EditNewsRequest {newImages = Just req} (Right _) = do
  rez <- mapM (SD.doesFileExist . DataTypes.image) req
  if and rez
    then do
      Logger.logDebug (News.hLogHandle h) "checkImageFilesExistIO: OK!"
      return $ Right r
    else do
      Logger.logError (News.hLogHandle h) ("ERROR " .< ErrorTypes.NotExistImageFileAddEditNews (ErrorTypes.InvalidContent "checkImageFileExistIO: BAD!  File: does not exist (No such file or directory) "))
      return . Left $
        ErrorTypes.NotExistImageFileAddEditNews $ ErrorTypes.InvalidContent []

-- | checkCategoryIdIO  check if there is a record with a given category id in the database ( only for the case with an editable category)
checkCategoryIdIO ::
  SQL.Connection ->
  News.Handle IO ->
  Either ErrorTypes.AddEditNewsError DataTypes.EditNewsRequest ->
  IO (Either ErrorTypes.AddEditNewsError DataTypes.EditNewsRequest)
checkCategoryIdIO _ _ (Left err) = return $ Left err
checkCategoryIdIO _ _ (Right r@DataTypes.EditNewsRequest {newCategoryId = Nothing}) =
  return $ Right r
checkCategoryIdIO conn h (Right r@DataTypes.EditNewsRequest {newCategoryId = Just categoryId}) = do
  res <-
    SQL.query
      conn
      [sql| SELECT category_path  FROM category WHERE category_id = ?|]
      (SQL.Only categoryId) ::
      IO [SQL.Only String]
  case res of
    [_] -> return $ Right r
    _ -> do
      Logger.logError
        (News.hLogHandle h)
        ( "ERROR "
            .< ErrorTypes.InvalidCategoryIdAddEditNews
              ( ErrorTypes.InvalidContent "checkCategoryIdIO: BAD! Not exists category with id "
              )
        )
      return $
        Left $
          ErrorTypes.InvalidCategoryIdAddEditNews $ ErrorTypes.InvalidContent []

checkPngImages ::
  News.Handle IO ->
  Either ErrorTypes.AddEditNewsError DataTypes.EditNewsRequest ->
  IO (Either ErrorTypes.AddEditNewsError DataTypes.EditNewsRequest)
checkPngImages _ (Left err) = return $ Left err
checkPngImages _ (Right r@DataTypes.EditNewsRequest {newImages = Nothing}) =
  return $ Right r
checkPngImages h (Right r@DataTypes.EditNewsRequest {newImages = Just req}) =
  if length (filter (\x -> DataTypes.format x == "png") req) == length req
    then do
      Logger.logDebug (News.hLogHandle h) "checkPngImages: OK!"
      return $ Right r
    else do
      Logger.logError (News.hLogHandle h) ("ERROR " .< ErrorTypes.NotPngImageAddEditNews (ErrorTypes.InvalidContent "checkPngImages: BAD!"))
      return . Left $
        ErrorTypes.NotPngImageAddEditNews $ ErrorTypes.InvalidContent []

checkBase64ImagesIO ::
  News.Handle IO ->
  Either ErrorTypes.AddEditNewsError DataTypes.EditNewsRequest ->
  IO (Either ErrorTypes.AddEditNewsError DataTypes.EditNewsRequest)
checkBase64ImagesIO _ (Left err) = return $ Left err
checkBase64ImagesIO _ (Right r@DataTypes.EditNewsRequest {newImages = Nothing}) =
  return $ Right r
checkBase64ImagesIO h (Right r@DataTypes.EditNewsRequest {newImages = Just req}) = do
  imageFiles <- mapM (B.readFile . DataTypes.image) req
  if length (filter Base64.isBase64 imageFiles) == length req
    then do
      Logger.logDebug (News.hLogHandle h) "checkBase64Image: OK!"
      return (Right r)
    else do
      Logger.logError (News.hLogHandle h) ("ERROR " .< ErrorTypes.NotBase64ImageAddEditNews (ErrorTypes.InvalidContent "checkBase64Image: BAD!"))
      return . Left $
        ErrorTypes.NotBase64ImageAddEditNews $ ErrorTypes.InvalidContent []
