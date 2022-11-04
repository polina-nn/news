{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- RestAPI
module Types.ApiTypes
  ( RestAPI
  ) where

import qualified Data.ByteString as B
import Data.ByteString.Lazy as LB (fromStrict)
import Data.Data (Typeable)
import qualified Data.Text as T
import Servant ((:<|>)(..), (:>), BasicAuth, Get, JSON, MimeRender(mimeRender))
import Servant.API
  ( Accept(contentType)
  , Capture
  , PlainText
  , Post
  , Put
  , QueryParam
  , ReqBody
  )
import qualified Types.DataTypes as DataTypes

type RestAPI
  --  server hello
   = Get '[ PlainText] T.Text :<|> BasicAuth "Create user by admin" DataTypes.User :> "login" :> "users" :> ReqBody '[ JSON] DataTypes.CreateUserRequest :> Post '[ JSON] DataTypes.User :<|> BasicAuth "Create category by admin" DataTypes.User :> "login" :> "category" :> ReqBody '[ JSON] DataTypes.CreateCategoryRequest :> Post '[ JSON] DataTypes.Category :<|> BasicAuth "Create news by author" DataTypes.User :> "login" :> "news" :> ReqBody '[ JSON] DataTypes.CreateNewsRequest :> Post '[ JSON] DataTypes.News :<|> BasicAuth "Create image by author" DataTypes.User :> "login" :> "image" :> ReqBody '[ JSON] DataTypes.CreateImageRequest :> Post '[ JSON] DataTypes.URI :<|> BasicAuth "Edit category by admin" DataTypes.User :> "login" :> "category" :> Capture "id" Int :> ReqBody '[ JSON] DataTypes.EditCategoryRequest :> Put '[ JSON] DataTypes.Category :<|> BasicAuth "Edit news by new's author" DataTypes.User :> "login" :> "news" :> Capture "id" Int :> ReqBody '[ JSON] DataTypes.EditNewsRequest :> Put '[ JSON] DataTypes.News
    --  get list of news with authentication, the author sees his unpublished news and all published
      :<|> BasicAuth "Get news by new's author" DataTypes.User :> "login" :> "news" :> QueryParam "created_at" DataTypes.DayAt :> QueryParam "created_until" DataTypes.DayUntil :> QueryParam "created_since" DataTypes.DaySince :> QueryParam "author" T.Text :> QueryParam "category_id" Int :> QueryParam "title" T.Text :> QueryParam "content" T.Text :> QueryParam "sort_by" DataTypes.SortBy :> QueryParam "offset" DataTypes.Offset :> QueryParam "limit" DataTypes.Limit :> Get '[ JSON] [DataTypes.News]
    --  get result of searching in list of news with authentication, the author sees his unpublished news and all published
      :<|> BasicAuth "Get searching in news by new's author" DataTypes.User :> "login" :> "news" :> "search" :> QueryParam "text" T.Text :> QueryParam "offset" DataTypes.Offset :> QueryParam "limit" DataTypes.Limit :> Get '[ JSON] [DataTypes.News]
    --  get list of users
      :<|> "users" :> QueryParam "offset" DataTypes.Offset :> QueryParam "limit" DataTypes.Limit :> Get '[ JSON] [DataTypes.User]
    -- get one image
      :<|> "image" :> Capture "id" Integer :> Get '[ IMAGE] B.ByteString
    --  get list of categories
      :<|> "category" :> QueryParam "offset" DataTypes.Offset :> QueryParam "limit" DataTypes.Limit :> Get '[ JSON] [DataTypes.Category]
    --  get list of news without authentication
      :<|> "news" :> QueryParam "created_at" DataTypes.DayAt :> QueryParam "created_until" DataTypes.DayUntil :> QueryParam "created_since" DataTypes.DaySince :> QueryParam "author" T.Text :> QueryParam "category_id" Int :> QueryParam "title" T.Text :> QueryParam "content" T.Text :> QueryParam "sort_by" DataTypes.SortBy :> QueryParam "offset" DataTypes.Offset :> QueryParam "limit" DataTypes.Limit :> Get '[ JSON] [DataTypes.News]
    --  get result of searching in list of news without authentication
      :<|> "news" :> "search" :> QueryParam "text" T.Text :> QueryParam "offset" DataTypes.Offset :> QueryParam "limit" DataTypes.Limit :> Get '[ JSON] [DataTypes.News]

-- | to display pictures:
data IMAGE
  deriving (Typeable)

instance MimeRender IMAGE B.ByteString where
  mimeRender _ = LB.fromStrict

instance Accept IMAGE where
  contentType _ = "image/png"
