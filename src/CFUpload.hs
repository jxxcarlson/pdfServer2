
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

testResponse = "{\n  \"result\": {\n    \"id\": \"49660d63-a43f-4011-1a7a-ff6435305d00\",\n    \"filename\": \"bird2.jpg\",\n    \"uploaded\": \"2023-03-16T23:08:22.768Z\",\n    \"requireSignedURLs\": false,\n    \"variants\": [\n      \"https://imagedelivery.net/9U-0Y4sEzXlO6BXzTnQnYQ/49660d63-a43f-4011-1a7a-ff6435305d00/public\"\n    ]\n  },\n  \"success\": true,\n  \"errors\": [],\n  \"messages\": []\n}"

testDecode str = Data.Aeson.eitherDecode $ BL.pack str :: Either String (Maybe CFUpload.CFUploadResponse)

myDecode str = Data.Aeson.decode $ BL.pack str :: Maybe CFUpload.CFUploadResponse 


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


instance FromJSON CFUploadResponse where
     parseJSON (Object v) =
        CFUploadResponse     <$>
            v .: "result"    <*> 
            v .: "success"   <*>
            v .: "errors"    <*>
            v .: "messages"

-- Tell Aeson how to convert a CFUploadResponse object to a JSON string.

instance FromJSON CFUploadResult where
    parseJSON = withObject "CFUploadResult" $ \o -> do
      id <- o .: Data.Text.pack  "id"
      filename <- o .: Data.Text.pack "filename"
      uploaded <- o .:  Data.Text.pack "uploaded"
      requireSignedURLs <- o .: Data.Text.pack "requireSignedURLs"
      variants <- o .: Data.Text.pack  "variants"
      return (CFUploadResult id filename uploaded requireSignedURLs variants) 
      



uploadResult :: CFUploadResponse -> CFUploadResult
uploadResult (CFUploadResponse result _ _ _) = result

getVariants :: CFUploadResult -> [String]
getVariants (CFUploadResult _ _ _ _ variants)  = variants

getUploadUrlFromResponse :: CFUploadResponse -> [String]
getUploadUrlFromResponse = getVariants . uploadResult

