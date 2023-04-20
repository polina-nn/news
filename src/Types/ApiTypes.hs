-- RestAPI
module Types.ApiTypes
  ( RestAPI,
  )
where

import qualified Data.ByteString as B
import Data.ByteString.Lazy as LB (fromStrict)
import Data.Data (Typeable)
import qualified Data.Text as T
import Servant (Get, JSON, MimeRender (mimeRender), (:<|>) (..), (:>))
import Servant.API
  ( Accept (contentType),
    Capture,
    PlainText,
    Post,
    Put,
    QueryParam,
    ReqBody,
  )
import Servant.API.Experimental.Auth (AuthProtect)
import qualified Types.DataTypes as DataTypes

type RestAPI =
  --  server hello
  Get '[PlainText] T.Text
    -- create user by admin
    :<|> AuthProtect "cookie-auth" :> "login" :> "users" :> ReqBody '[JSON] DataTypes.CreateUserRequest :> Post '[JSON] DataTypes.User
    -- create category by admin
    :<|> AuthProtect "cookie-auth" :> "login" :> "category" :> ReqBody '[JSON] DataTypes.CreateCategoryRequest :> Post '[JSON] DataTypes.Category
    -- create news by author
    :<|> AuthProtect "cookie-auth" :> "login" :> "news" :> ReqBody '[JSON] DataTypes.CreateNewsRequest :> Post '[JSON] DataTypes.News
    -- create image by author
    :<|> AuthProtect "cookie-auth" :> "login" :> "image" :> ReqBody '[JSON] DataTypes.CreateImageRequest :> Post '[JSON] DataTypes.URI
    -- edit category by admin
    :<|> AuthProtect "cookie-auth" :> "login" :> "category" :> Capture "id" (DataTypes.Id DataTypes.Category) :> ReqBody '[JSON] DataTypes.EditCategoryRequest :> Put '[JSON] DataTypes.Category
    -- edit news by new's author"
    :<|> AuthProtect "cookie-auth" :> "login" :> "news" :> Capture "id" (DataTypes.Id DataTypes.News) :> ReqBody '[JSON] DataTypes.EditNewsRequest :> Put '[JSON] DataTypes.News
    --  get list of news with authentication, the author sees his unpublished news and all published
    :<|> AuthProtect "cookie-auth" :> "login" :> "news"
      :> QueryParam "created_at" DataTypes.DayAt
      :> QueryParam "created_until" DataTypes.DayUntil
      :> QueryParam "created_since" DataTypes.DaySince
      :> QueryParam "author" T.Text
      :> QueryParam "category_id" (DataTypes.Id DataTypes.Category)
      :> QueryParam "title" T.Text
      :> QueryParam "content" T.Text
      :> QueryParam "sort_by" DataTypes.SortBy
      :> QueryParam "offset" DataTypes.Offset
      :> QueryParam "limit" DataTypes.Limit
      :> Get '[JSON] [DataTypes.News]
    --  get result of searching in list of news with authentication, the author sees his unpublished news and all published
    :<|> AuthProtect "cookie-auth" :> "login" :> "news" :> "search" :> QueryParam "text" T.Text :> QueryParam "offset" DataTypes.Offset :> QueryParam "limit" DataTypes.Limit :> Get '[JSON] [DataTypes.News]
    --  get list of users
    :<|> "users" :> QueryParam "offset" DataTypes.Offset :> QueryParam "limit" DataTypes.Limit :> Get '[JSON] [DataTypes.User]
    -- get one image
    :<|> "image" :> Capture "id" (DataTypes.Id DataTypes.Image) :> Get '[IMAGE] B.ByteString
    --  get list of categories
    :<|> "category" :> QueryParam "offset" DataTypes.Offset :> QueryParam "limit" DataTypes.Limit :> Get '[JSON] [DataTypes.Category]
    --  get list of news without authentication
    :<|> "news"
      :> QueryParam "created_at" DataTypes.DayAt
      :> QueryParam "created_until" DataTypes.DayUntil
      :> QueryParam "created_since" DataTypes.DaySince
      :> QueryParam "author" T.Text
      :> QueryParam "category_id" (DataTypes.Id DataTypes.Category)
      :> QueryParam "title" T.Text
      :> QueryParam "content" T.Text
      :> QueryParam "sort_by" DataTypes.SortBy
      :> QueryParam "offset" DataTypes.Offset
      :> QueryParam "limit" DataTypes.Limit
      :> Get '[JSON] [DataTypes.News]
    --  get result of searching in list of news without authentication
    :<|> "news" :> "search" :> QueryParam "text" T.Text :> QueryParam "offset" DataTypes.Offset :> QueryParam "limit" DataTypes.Limit :> Get '[JSON] [DataTypes.News]

-- | to display pictures:
data IMAGE
  deriving (Typeable)

instance MimeRender IMAGE B.ByteString where
  mimeRender _ = LB.fromStrict

instance Accept IMAGE where
  contentType _ = "image/png"
