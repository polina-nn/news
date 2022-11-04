module Handle
  ( handleSpec,
    appConfigSpec,
    appConfigSpecForLimit,
    handleSpecForLimit,
    appShowLimit,
  )
where

import Control.Monad.Identity (Identity)
import qualified Logger
import News (AppConfig (..), DbConfig (..), Handle (..), Scheme (..), URIConfig (..))

-- turn of logging
handleSpec :: News.Handle Identity
handleSpec =
  News.Handle
    { hLogHandle = logHandleSpec,
      hAppConfig = appConfigSpec,
      hDbConfig = dbConfigSpec,
      hURIConfig = uRIConfigSpec
    }

logHandleSpec :: Logger.Handle Identity
logHandleSpec =
  Logger.Handle
    { Logger.hLowLevelLog =
        \_ _ -> return ()
    }

-- AppConfigLimit appShowLimit = 3 for testing offset
appConfigSpec :: AppConfig
appConfigSpec =
  AppConfig
    { appPort = 8080,
      appShowLimit = 3
    }

-- AppConfigLimit  appShowLimit = 100  (passed 100 tests for all cases for limit ) Tests fail because there are not enough test cases if appShowLimit is small
handleSpecForLimit :: News.Handle Identity
handleSpecForLimit =
  News.Handle
    { hLogHandle = logHandleSpec,
      hAppConfig = appConfigSpecForLimit,
      hDbConfig = dbConfigSpec,
      hURIConfig = uRIConfigSpec
    }

appConfigSpecForLimit :: AppConfig
appConfigSpecForLimit =
  AppConfig
    { appPort = 8080,
      appShowLimit = 100
    }

-- | dbConfigSpec - define, but do not use in tests
dbConfigSpec :: News.DbConfig
dbConfigSpec =
  News.DbConfig
    { News.dbHost = "localhost",
      News.dbName = "tiny",
      News.user = "postgres",
      News.password = "postgres123",
      News.dbPort = "5432",
      News.noOfStripes = 2, --- stripes https://docs.servant.dev/en/stable/cookbook/db-postgres-pool/PostgresPool.html
      News.idleTime = 60, --- unused connections are kept open for a minute
      News.stripeSize = 10 --- max. 10 connections open per stripe
    }

-- | uRIConfigSpec - define, but do not use in tests
uRIConfigSpec :: News.URIConfig
uRIConfigSpec =
  News.URIConfig
    { News.uriScheme = Http,
      News.uriHost = "localhost",
      News.uriPort = 8080
    }