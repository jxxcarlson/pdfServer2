{-# LANGUAGE DeriveGeneric #-}

module CFOnetimeUrl where


import Data.Aeson
import GHC.Generics
import Data.Text

data CFOnetimeUrl = CFOnetimeUrl
  {
    result   :: CFOnetimeUrlResult,
    success  :: Bool,
    errors   :: [String],
    messages :: [String]
  } deriving (Show, Generic)

data CFOnetimeUrlResult = CFOnetimeUrlResult {
    id             :: String,
    uploadURL      :: String
  } deriving (Show, Generic)


instance FromJSON CFOnetimeUrl

-- Tell Aeson how to convert a CFOnetimeUrlResult object to a JSON string.
instance FromJSON CFOnetimeUrlResult


onetimeResult :: CFOnetimeUrl -> CFOnetimeUrlResult
onetimeResult (CFOnetimeUrl result  _ _ _) = result

uploadURLFromResult :: CFOnetimeUrlResult -> String
uploadURLFromResult (CFOnetimeUrlResult _ uploadURL) = uploadURL

uploadURLFromResponse :: CFOnetimeUrl -> String
uploadURLFromResponse = uploadURLFromResult . onetimeResult
