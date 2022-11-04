{-# LANGUAGE OverloadedStrings #-}

module MainCR
  ( mainCR
  ) where

-- mainCR - generate hash. You must  manually add the fist user-admin to the data base
import qualified Crypto.Hash
import qualified Data.ByteArray.Encoding as BAE
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO

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
--}
{--
for convenience, the login == the password in the database
You entered: "123"     SHA256 hash: "pmWkWSBCL51Bfkhn79xPuKBKHz//H6B+mY6G9/eieuM="
You entered: "polina"  SHA256 hash: "s0zUqXz8ZkllWgC/qc0/s6y19z5ZSY7gR1tFR9tRYTE="
You entered: "12345"   SHA256 hash: "WZRHGrsBESr8wYFZ9sx0tPURuZgG2lmzyvWpwXPKz8U="
You entered: "qwer"    SHA256 hash: "9vLqj0XYoFfJVmoz+ZR02i5camYE1zYSFlDicwxvsKM="
You entered: "loginUser0" SHA256 hash: "xgNiL+Z7Pf59ymZpu1c7+xqsa3OKMecdgprtmbHTomY="
You entered: "loginUser2" SHA256 hash: "8Iv997nYKOz3rYWElFW3C4wEcLCcQNStWLn+mdvvrAo="
You entered: "loginAdmin1" SHA256 hash: "ar8UyphPGVNlg5RnUXS8k+afIXuft6TznbJzBUC4OJg="
You entered: "loginAdminAuthor1" SHA256 hash: "eaZhQj2/LXp33ZwSAzKDjVm0wLls1fhJB3MS1xujwus="
You entered: "loginAuthor1" SHA256 hash: "+FJ1IUwnADTwgXj1qKYvrkLzKTkX83Q7og2Q/26EzBU="
--}
