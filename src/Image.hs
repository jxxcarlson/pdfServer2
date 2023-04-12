{-# LANGUAGE OverloadedStrings #-}

module Image where

import Control.Applicative ((<*>), (<$>), empty)
import Control.Monad (mzero, void)
import GHC.IO.Exception

import System.Process
import System.Environment (getEnv)
import System.FilePath
import System.Path

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import qualified Network.HTTP.Client.MultipartFormData  as MultiPart

import Data.List.Split
import Data.List.Utils (replace)
import Data.Text.Lazy ( Text, pack )
import Data.Text
import Data.Text.Lazy.Encoding
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy as TL

import Data.Aeson

import qualified CFUpload
import qualified CFOnetimeUrl
import qualified CFImage




uploadTheImage :: String -> [Char] -> IO BL.ByteString
uploadTheImage uploadUrl filename  = do
    manager <- newTlsManager
    request <- parseRequest uploadUrl
    let request' = request {method = "POST"}
    let requestFile = "cf-image/" ++ filename 
    request'' <- MultiPart.formDataBody [ MultiPart.partFileSource "file" requestFile] request'
    response <- httpLbs request'' manager
    let cmd = "rm " ++ requestFile
    system cmd >>= \exitCode -> print exitCode
    return $ responseBody response



requestCFToken :: IO (String)
requestCFToken = do
    cloudFlareAccountId <- getEnv "CF_ACCOUNT_ID"
    cloudFlareAPIKey <- getEnv "CF_API_KEY"
    manager <- newTlsManager 
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


byteStringToText byteString = TL.unpack $ TLE.decodeUtf8 byteString

