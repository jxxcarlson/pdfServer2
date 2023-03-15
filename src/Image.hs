{-# LANGUAGE OverloadedStrings #-}

module Image where

import Data.Text.Lazy ( Text, pack )
import Data.Text
import Data.Text.Lazy.Encoding
import Data.Aeson
import Control.Applicative
import Data.List.Split
import Data.List.Utils (replace)
import Control.Applicative ((<*>), (<$>), empty)
import Control.Monad (mzero)
import GHC.IO.Exception
import System.Process
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy as TL

data CFImage = CFImage
      {
        url        :: String
      , filename   :: String
      , username   :: String
      } deriving Show

data CFUploadResponse = CFUploadResponse
  {
    result   :: CFUploadData,
    success  ::Bool,
    errors   :: [String],
    messages :: [String]
  } deriving Show

data CFUploadData = CFUploadData {
    id        :: String,
    uploadUrl :: String
  } deriving Show

-- Tell Aeson how to convert a CFUploadResponse object to a JSON string.
instance FromJSON CFUploadData where
  parseJSON = withObject "CFUploadData" $ \o -> do
      id <- o .: "id"
      uploadURL <- o .: "uploadURL"
      return $ CFUploadData id uploadURL 


-- instance FromJSON CFUploadData where
--   parseJSON (Object v) =
--     CFUploadData
--       <$> v .: "id"
--       <*> v .: "uploadURL"
--     return $ CFUploadData id uploadURL


instance FromJSON CFUploadResponse where
     parseJSON (Object v) =
        CFUploadResponse     <$>
            v .: "result"    <*> 
            v .: "success"   <*>
            v .: "errors"    <*>
            v .: "messages"



uploadResult :: CFUploadResponse -> CFUploadData
uploadResult (CFUploadResponse result  _ _ _) = result

getUploadUrl :: CFUploadData -> String
getUploadUrl (CFUploadData _ url)  = url

getUploadUrlFromResult :: CFUploadResponse -> String
getUploadUrlFromResult = getUploadUrl . uploadResult

requestCFToken :: IO (String)
requestCFToken = do
    manager <- newTlsManager -- create a new manager
    let url = "https://api.cloudflare.com/client/v4/accounts/" ++ cloudFlareAccountId ++ "/images/v2/direct_upload"
        request = (parseRequest_ url)
            { method = "POST"
            , requestHeaders = [("Content-Type", "application/json"), 
              ("Authorization", BL.toStrict $ B.toLazyByteString $ "Bearer " <> B.stringUtf8 cloudFlareAPIKey) 
            ]
            }
    response <- httpLbs request manager 
    let cfUploadResponse = (Data.Aeson.decode $ responseBody response)  :: Maybe CFUploadData
    case cfUploadResponse of 
      Nothing -> return $  show "Could not decode Cloudflare's response: " ++ (show $ responseBody response)
      Just daata -> return $ show daata


jsonStr = "{\n  \"result\": {\n    \"id\": \"7bff08f3-9c52-4d2f-21d1-923fae172300\",\n    \"uploadURL\": \"https://upload.imagedelivery.net/9U-0Y4sEzXlO6BXzTnQnYQ/7bff08f3-9c52-4d2f-21d1-923fae172300\"\n  },\n  \"success\": true,\n  \"errors\": [],\n  \"messages\": []\n}"

-- {
--   "result": {
--     "id": "7bff08f3-9c52-4d2f-21d1-923fae172300",
--     "uploadURL": "https://upload.imagedelivery.net/9U-0Y4sEzXlO6BXzTnQnYQ/7bff08f3-9c52-4d2f-21d1-923fae172300"
--   },
--   "success": true,
--   "errors": [],
--   "messages": []
-- }

 

-- requestCFToken :: IO (String)
-- requestCFToken = do
--     manager <- newTlsManager -- create a new manager
--     let url = "https://api.cloudflare.com/client/v4/accounts/" ++ cloudFlareAccountId ++ "/images/v2/direct_upload"
--         -- payload = object ["name" .= "Alice", "age" .= (25 :: Int)]
--         request = (parseRequest_ url)
--             { method = "POST"
--             -- , requestBody = RequestBodyLBS $ encode payload
--             , requestHeaders = [("Content-Type", "application/json"), 
--               ("Authorization", BL.toStrict $ B.toLazyByteString $ "Bearer " <> B.stringUtf8 cloudFlareAPIKey) 
--             ]
--             }
--     response <- httpLbs request manager -- send the request
--     let cfUploadResponse = (Data.Aeson.decode $ responseBody response)  :: Maybe CFUploadData
--     case cfUploadResponse of 
--       Nothing -> return $  show "Could not decode Cloudflare's response: " ++ (show $ responseBody response)
--       Just daata -> return $ show daata



    -- putStrLn $ show $ responseBody response
    -- document <- jsonData :: ActionM Document
    -- return $ responseBody response
    

cloudFlareAPIKey = "wmBxrHLMHFfBa4P5M6gGfS-w8dYJELRu74dA_wcy"
cloudFlareAccountId = "b9ef57c1554dd097a1cb697f5809acd9"


prepareCFImage :: CFImage -> IO()
prepareCFImage cfImage =
    do
      downloadCFImage cfImage >> return () -- write the image file to ./cf-image
  

downloadCFImage :: CFImage -> IO (GHC.IO.Exception.ExitCode)
downloadCFImage image =
   system ("wget -O " ++ "cf-image/" ++ (filename image) ++ " " ++  (url image))


-- Tell Aeson how to create a CFImage object from JSON string.
instance FromJSON CFImage where
     parseJSON (Object v) = CFImage <$>
                            v .:  (Data.Text.pack "url") <*> 
                            v .:  (Data.Text.pack "filename") <*>
                            v .:  (Data.Text.pack "username") 
                           
 

-- Tell Aeson how to convert a CFImage object to a JSON string.
instance ToJSON CFImage where
     toJSON (CFImage url filename username) =
         object ["url" .= url,
                 "filename" .= filename,
                 "username" .= username
                 ]


byteStringToText byteString = TL.unpack $ TLE.decodeUtf8 byteString


-- instance FromJSON CFUploadResponse where
--   parseJSON = withObject "Response" $ \o -> do
--     result <- o .: "result"
--     success <- o .: "success"
--     errors <- o .: "errors"
--     messages <- o .: "messages"
--     return $ CFUploadResponse result success errors messages
