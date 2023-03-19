{-# LANGUAGE OverloadedStrings #-}

module CFUpload where


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

testResponse = "{\n  \"result\": {\n    \"id\": \"673996fb-4d26-4332-6e6b-e8bf7b608500\",\n    \"filename\": \"bird2.jpg\",\n    \"uploaded\": \"2023-03-18T22:53:56.705Z\",\n    \"requireSignedURLs\": false,\n    \"variants\": [\n      \"https://imagedelivery.net/9U-0Y4sEzXlO6BXzTnQnYQ/673996fb-4d26-4332-6e6b-e8bf7b608500/public\"\n    ]\n  },\n  \"success\": true,\n  \"errors\": [],\n  \"messages\": []\n}"

myDecode str = Data.Aeson.eitherDecode $ BL.pack str :: Either String (Maybe CFUpload.CFUploadResponse)

-- myDecode str = Data.Aeson.decode $ BL.pack str :: Maybe CFUpload.CFUploadResponse 

-- {
--   "result": {
--     "id": "2e339309-f421-476a-4766-54b384761100",
--     "filename": "bird2.jpg",
--     "uploaded": "2023-03-19T05:40:38.491Z",
--     "requireSignedURLs": false,
--     "variants": [
--       "https://imagedelivery.net/9U-0Y4sEzXlO6BXzTnQnYQ/2e339309-f421-476a-4766-54b384761100/public"
--     ]
--   },
--   "success": true,
--   "errors": [],
--   "messages": []
-- }  
      
-- CF UPLOAD RESPONSE

data CFUploadResponse = CFUploadResponse
  {
    result   :: CFUploadResult,
    success  :: Bool,
    errors   :: [String],
    messages :: [String]
  } deriving Show

data CFUploadResult = CFUploadResult {
    id                :: String,
    filename          :: String,
    uploaded          :: String,
    requireSignedURLs :: Bool,
    variants          :: [String]
  } deriving Show

-- Tell Aeson how to convert a CFUploadResponse object to a JSON string.
instance FromJSON CFUploadResponse where
    parseJSON = withObject "CFUploadResponse" $ \o -> do
      result <- o .: "result"
      success <- o .: "success"
      errors <- o .: "errors"
      messages <- o .: "messages"
      return (CFUploadResponse result success errors messages)
    -- parseJSON other = fail ("Invalid JSON for CFUploadResponse: " ++ show other)

instance FromJSON CFUploadResult where
    parseJSON = withObject "CFUploadResult" $ \o -> do
      id <- o .: "id"
      filename <- o .: "filename"
      uploaded <- o .: "uploaded"
      requireSignedURLs <- o .: "requireSignedURLs"
      variants <- o .: "variants"
      return (CFUploadResult id filename uploaded requireSignedURLs variants)
  

uploadResult :: CFUploadResponse -> CFUploadResult
uploadResult (CFUploadResponse result _ _ _) = result

getVariants :: CFUploadResult -> [String]
getVariants (CFUploadResult _ _ _ _ variants)  = variants

getUploadUrlFromResponse :: CFUploadResponse -> [String]
getUploadUrlFromResponse = getVariants . uploadResult

