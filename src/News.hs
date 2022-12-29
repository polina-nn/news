-- | Handle for Config and Logger
module News where

--import Logger ((.<))
import qualified Logger

--import qualified Types.ConfigurationTypes
data Handle m = Handle
  { hLogHandle :: Logger.Handle m,
    hAppConfig :: AppConfig,
    hDbConfig :: DbConfig,
    hURIConfig :: URIConfig
  }

type AppConfigLimit = Int

data AppConfig = AppConfig
  { appPort :: Int,
    appShowLimit :: AppConfigLimit
  }
  deriving (Show, Eq)

data DbConfig = DbConfig
  { dbHost :: String,
    dbName :: String,
    user :: String,
    password :: String,
    dbPort :: String,
    noOfStripes :: Int,
    idleTime :: Int,
    stripeSize :: Int
  }
  deriving (Show, Eq)

-- | URI scheme to use (when requesting an image, I return the URI)
data Scheme
  = -- | http://
    Http
  | -- | https://
    Https
  deriving (Eq)

instance Show Scheme where
  show Http = "http:/"
  show Https = "https:/"

-- | Simple data type to represent the target of HTTP requests
--   for servant's automatically-generated clients.
data URIConfig = URIConfig
  { -- | URI scheme to use
    uriScheme :: Scheme,
    -- | host (eg "haskell.org")
    uriHost :: String,
    -- | port (eg 80)
    uriPort :: Int
  }
  deriving (Eq)

instance Show URIConfig where
  show (URIConfig uriScheme' "localhost" uriPort') =
    show uriScheme' ++ "/localhost:" ++ show uriPort'
  show (URIConfig uriScheme' uriHost' uriPort') =
    show uriScheme' ++ "/" ++ uriHost' ++ "/" ++ show uriPort'
