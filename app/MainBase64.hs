module MainBase64
  ( mainBase64,
  )
where

import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as Base64

-- | mainBase64 --code image in base64 from png
-- _image/red.png
-- to display in the browser in place ??? set file content to base64
-- <img src="data:image/png;base64,???" />
mainBase64 :: IO ()
mainBase64 = do
  putStr "Enter file path "
  fileName <- getLine
  imageFile <- B.readFile fileName
  let base64FileName =
        Prelude.mconcat [takeWhile (/= '.') fileName, "_base64.png"]
  B.writeFile base64FileName (Base64.encodeBase64' imageFile)
  putStrLn "Done!"
