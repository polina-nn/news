-- |  EndPoints.Lib.Lib - library of helper pure  functions for EndPoints
module EndPoints.Lib.Lib
  ( checkUserAdmin,
    checkUserAuthor,
    currentDay,
    hashed,
    imageIdToURI,
    imagesURIs,
    toUser,
  )
where

import Control.Monad.Trans.Class (MonadTrans (lift))
import qualified Control.Monad.Trans.Except as EX
import qualified Crypto.Hash
import qualified Data.ByteArray.Encoding as BAE
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Time as TIME
import Logger (logDebug, logError, (.<))
import qualified News
import qualified Types.DataTypes as DataType
import qualified Types.DataTypes as DataTypes
import qualified Types.ErrorTypes as ErrorTypes

checkUserAdmin ::
  Monad m =>
  News.Handle m ->
  DataTypes.User ->
  EX.ExceptT ErrorTypes.InvalidAdminPermission m DataTypes.User
checkUserAdmin h r@DataTypes.User {..} =
  if userAdmin
    then do
      lift $ Logger.logDebug (News.hLogHandle h) "checkUserAdmin: OK!"
      return r
    else do
      lift $
        Logger.logError
          (News.hLogHandle h)
          ( "ERROR "
              .< ErrorTypes.InvalidAdminPermission
                "checkUserAdmin: BAD! User is not admin. Invalid Permission for this request."
          )
      EX.throwE $ ErrorTypes.InvalidAdminPermission []

checkUserAuthor ::
  Monad m =>
  News.Handle m ->
  DataTypes.User ->
  EX.ExceptT ErrorTypes.InvalidAuthorPermission m DataTypes.User
checkUserAuthor h r@DataTypes.User {..} =
  if userAuthor
    then do
      lift $ Logger.logDebug (News.hLogHandle h) "checkUserAuthor: OK!"
      EX.except (Right r)
    else do
      lift $
        Logger.logError
          (News.hLogHandle h)
          ( "ERROR "
              .< ErrorTypes.InvalidAuthorPermission
                "checkUserAuthor: BAD! User is not author. Invalid Permission for this request."
          )
      EX.throwE $ ErrorTypes.InvalidAuthorPermission []

-- | imageIdToURI - create URI of one image
imageIdToURI :: News.Handle IO -> Int -> DataTypes.URI
imageIdToURI h idIm = show uriBegin ++ show uriEnd
  where
    uriBegin = News.hURIConfig h
    uriEnd = DataTypes.URI' {uriPath = "image", uriId = idIm}

-- | imagesURIs - create URI of images
imagesURIs :: News.Handle IO -> [Int] -> [DataTypes.URI]
imagesURIs _ [] = []
imagesURIs h xs = map (imageIdToURI h) xs

-- | currentDay -- return current data, used when creating user or news
currentDay :: IO TIME.Day
currentDay = fmap TIME.utctDay TIME.getCurrentTime

-- | hashed -- return hashed string,used when create password for user
hashed :: String -> String
hashed xs = BSC8.unpack $ hashedByteStr $ T.pack xs
  where
    hashedByteStr :: T.Text -> BSC8.ByteString
    hashedByteStr text =
      BAE.convertToBase
        BAE.Base64
        (Crypto.Hash.hashWith Crypto.Hash.SHA256 $ TE.encodeUtf8 text)

toUser :: (T.Text, String, Bool, Bool, TIME.Day) -> DataTypes.User
toUser (userName, userLogin, userAdmin, userAuthor, userCreated) =
  let userPassword = Nothing
   in DataTypes.User {..}
