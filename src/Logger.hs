-- | The logger interface module. It should not define a specific
-- implementation.
module Logger
  ( Handle (..),
    Level (..),
    logDebug,
    logInfo,
    logWarning,
    logError,
    (.<),
  )
where

import qualified Data.Configurator.Types as C
import qualified Data.Text as T
import Text.Read (readMaybe)

-- | The logger handle. This is a public logger interface that can
-- have different implementations. You can use it everywhere.
newtype Handle m = Handle
  { hLowLevelLog :: Level -> T.Text -> m ()
  }

data Level
  = -- | Debug messages
    Debug
  | -- | Notable information that requires no immediate action.
    Info
  | -- | Something is probably wrong, and we should investigate.
    Warning
  | -- | Something is wrong and immediate action is required.
    Error
  deriving (Show, Eq, Ord, Read)

instance C.Configured Level where
  convert (C.String str) = readMaybe (T.unpack str)
  convert _ = Nothing

logDebug, logInfo, logWarning, logError :: Handle m -> T.Text -> m ()
logDebug h = hLowLevelLog h Debug
logInfo h = hLowLevelLog h Info
logWarning h = hLowLevelLog h Warning
logError h = hLowLevelLog h Error

-- | Concatenates a text and an instance of 'Show'. This is a
-- convenience function to make logger function applications more
-- concise:
--
-- > Log.logError (hLogger h) "The error code is " .< e
(.<) :: (Show a) => T.Text -> a -> T.Text
text .< a = text <> T.pack (show a)

infixr 7 .<
