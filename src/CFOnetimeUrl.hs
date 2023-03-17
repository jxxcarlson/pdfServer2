{-# LANGUAGE OverloadedStrings #-}

module CFOnetimeUrl where

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



-- ONE TIME URL RESPONSE

data CFOnetimeUrl = CFOnetimeUrl
  {
    result   :: CFOnetimeUrlResult,
    success  :: Bool,
    errors   :: [String],
    messages :: [String]
  } deriving Show

data CFOnetimeUrlResult = CFOnetimeUrlResult {
    id             :: String,
    uploadUrl      :: String
  } deriving Show


instance FromJSON CFOnetimeUrl where
     parseJSON (Object v) =
        CFOnetimeUrl         <$>
            v .: "result"    <*> 
            v .: "success"   <*>
            v .: "errors"    <*>
            v .: "messages"

-- Tell Aeson how to convert a CFOnetimeUrlResult object to a JSON string.
instance FromJSON CFOnetimeUrlResult where
  parseJSON = withObject "CFOnetimeUrlResult" $ \o -> do
          id         <-  o .: "id"
          uploadUrl  <- o .: "uploadURL"
          return (CFOnetimeUrlResult id uploadUrl) 


onetimeResult :: CFOnetimeUrl -> CFOnetimeUrlResult
onetimeResult (CFOnetimeUrl result  _ _ _) = result

uploadUrlFromResult :: CFOnetimeUrlResult -> String
uploadUrlFromResult (CFOnetimeUrlResult _ uploadUrl) = uploadUrl



uploadUrlFromResponse :: CFOnetimeUrl -> String
uploadUrlFromResponse = uploadUrlFromResult . onetimeResult
