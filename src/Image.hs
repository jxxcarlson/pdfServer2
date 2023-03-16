{-# LANGUAGE OverloadedStrings #-}

module Image where

import System.Environment (getEnv)
import Data.Text.Lazy ( Text, pack )
import Data.Text
import Data.Text.Lazy.Encoding
import Data.Aeson
import Control.Applicative
import Data.List.Split
import Data.List.Utils (replace)
import Control.Applicative ((<*>), (<$>), empty)
import Control.Monad (mzero, void)
import GHC.IO.Exception
import System.Process
import System.FilePath
import System.Path
import Network.HTTP.Client
import Network.HTTP.Client.TLS
-- import Network.HTTP.Client.MultipartFormData


import qualified Data.ByteString.Lazy.Char8 as LBS
-- import qualified Network.HTTP.Client        as Client
-- import qualified Network.HTTP.Client.TLS    as TLS
import qualified Network.HTTP.Client.MultipartFormData  as MultiPart


import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy as TL



data CFImage = CFImage
      {
        url             :: String
      , imageFilename   :: String
      , username        :: String
      } deriving Show

getFilenameFromImage :: CFImage -> String
getFilenameFromImage (CFImage _ imageFilename _) = imageFilename

getUrlFromImage :: CFImage -> String
getUrlFromImage (CFImage url _ _ ) = url

updateCFImage :: String -> CFImage -> CFImage
updateCFImage newUrl cfImage = 
     cfImage { url = newUrl }


data CFUploadResponse = CFUploadResponse
  {
    result   :: CFUploadData,
    success  ::Bool,
    errors   :: [String],
    messages :: [String]
  } deriving Show

data CFUploadData = CFUploadData {
    id                :: String,
    filename          :: String,
    uploaded          :: String,
    requireSignedURLs :: Bool,
    variants          :: [String]
  } deriving Show


-- Tell Aeson how to convert a CFUploadResponse object to a JSON string.
instance FromJSON CFUploadData where
  parseJSON = withObject "CFUploadData" $ \o -> do
      id <- o .: "id"
      filename <- o .: "filename"
      uploaded <- o .:  "uploaded"
      requireSignedURLs <- o .: "requireSignedURLs"
      variants <- o .: "variants"
      return (CFUploadData id filename uploaded requireSignedURLs variants) 


-- {
--   "result": {
--     "id": "945764ee-63c2-4036-c6fd-39c7576f7700",
--     "filename": "bird2.jpg",
--     "uploaded": "2023-03-16T16:22:10.542Z",
--     "requireSignedURLs": false,
--     "variants": [
--       "https://imagedelivery.net/9U-0Y4sEzXlO6BXzTnQnYQ/945764ee-63c2-4036-c6fd-39c7576f7700/public"
--     ]
--   },
--   "success": true,
--   "errors": [],
--   "messages": []
-- }



instance FromJSON CFUploadResponse where
     parseJSON (Object v) =
        CFUploadResponse     <$>
            v .: "result"    <*> 
            v .: "success"   <*>
            v .: "errors"    <*>
            v .: "messages"

uploadResult :: CFUploadResponse -> CFUploadData
uploadResult (CFUploadResponse result  _ _ _) = result

getUploadUrl :: CFUploadData -> [String]
getUploadUrl (CFUploadData _ _ _ _ variants)  = variants

getUploadUrlFromResponse :: CFUploadResponse -> [String]
getUploadUrlFromResponse = getUploadUrl . uploadResult



uploadTheImage :: String -> String -> IO (String)
uploadTheImage uploadUrl filename  = do
    manager <- newTlsManager
    request <- parseRequest uploadUrl
    let request' = request {method = "POST"}
    let requestFile = "cf-image/" ++ filename 
    request'' <- MultiPart.formDataBody [ MultiPart.partFileSource "file" requestFile] request'
    response <- httpLbs request'' manager
    -- LBS.putStrLn $ responseBody response
    return $  show $ responseBody response



requestCFToken :: IO (String)
requestCFToken = do
    cloudFlareAccountId <- getEnv "CF_ACCOUNT_ID"
    cloudFlareAPIKey <- getEnv "CF_API_KEY"
    manager <- newTlsManager -- create a new manager
    let url = "https://api.cloudflare.com/client/v4/accounts/" ++ cloudFlareAccountId ++ "/images/v2/direct_upload"
        request = (parseRequest_ url)
            { method = "POST"
            , requestHeaders = [("Content-Type", "application/json"), 
              ("Authorization", BL.toStrict $ B.toLazyByteString $ "Bearer " <> B.stringUtf8 cloudFlareAPIKey) 
            ]
            }
    response <- httpLbs request manager 
    let cfUploadResponse = (Data.Aeson.decode  (responseBody response))  :: Maybe CFUploadResponse
    case cfUploadResponse of 
      Nothing -> return $  show "Could not decode Cloudflare's response: " ++ (show $ responseBody response)
      Just daata -> return $ show (getUploadUrlFromResponse daata)
  


prepareCFImage :: CFImage -> IO()
prepareCFImage cfImage =
    do
      downloadCFImage cfImage >> return () -- write the image file to ./cf-image
  

downloadCFImage :: CFImage -> IO (GHC.IO.Exception.ExitCode)
downloadCFImage image =
   system ("wget -O " ++ "cf-image/" ++ (imageFilename image) ++ " " ++  (url image))


-- Tell Aeson how to create a CFImage object from JSON string.
instance FromJSON CFImage where
     parseJSON (Object v) = CFImage <$>
                            v .:  (Data.Text.pack "url") <*> 
                            v .:  (Data.Text.pack "imageFilename") <*>
                            v .:  (Data.Text.pack "username") 
                           
 

-- Tell Aeson how to convert a CFImage object to a JSON string.
instance ToJSON CFImage where
     toJSON (CFImage url filename username) =
         object ["url" .= url,
                 "imageFilename" .= filename,
                 "username" .= username
                 ]


byteStringToText byteString = TL.unpack $ TLE.decodeUtf8 byteString

