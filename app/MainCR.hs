module MainCR
  ( mainCR
  ) where

import qualified Crypto.Hash
import qualified Data.ByteArray.Encoding as BAE
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO

-- | mainCR - generate hash. You must  manually add the fist user-admin to the data base
-- for convenience, the login == the password in the database
-- You entered: "polina"  SHA256 hash: "s0zUqXz8ZkllWgC/qc0/s6y19z5ZSY7gR1tFR9tRYTE="
-- You entered: "qwer"    SHA256 hash: "9vLqj0XYoFfJVmoz+ZR02i5camYE1zYSFlDicwxvsKM="
mainCR :: IO ()
mainCR = do
  putStr "Enter password: "
  text <- TIO.getLine
  let bs = TE.encodeUtf8 text
  putStrLn $ "You entered: " ++ show bs
  let digest =
        BAE.convertToBase
          BAE.Base64
          (Crypto.Hash.hashWith Crypto.Hash.SHA256 bs)
  putStrLn $ "SHA256 hash: " ++ show (digest :: BSC8.ByteString)
