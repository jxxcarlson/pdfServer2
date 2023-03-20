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

import qualified CFUpload
import qualified CFOnetimeUrl
import qualified CFImage

import Data.Text.Encoding (decodeUtf8)

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
    let responseBody' =  BL.unpack $ responseBody response
    let cfOnetimeUrlResponse = (Data.Aeson.eitherDecode (responseBody response)  :: Either String (Maybe CFOnetimeUrl.CFOnetimeUrl))
    case cfOnetimeUrlResponse of 
      Left err -> return  err
      Right stuff -> 
        case stuff of
          Nothing -> return "Werid! got Nothing when I shouldn't have"
          Just realStuff -> return (CFOnetimeUrl.uploadURLFromResponse realStuff)
    -- return $ show $ cfOnetimeUrlResponse 
  



byteStringToText byteString = TL.unpack $ TLE.decodeUtf8 byteString

