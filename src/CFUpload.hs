{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module CFUpload where


import Data.Aeson
import Data.Aeson.Types
import GHC.Generics
import Data.Text
import Data.List
import qualified Data.ByteString.Lazy as LB (ByteString)

import qualified Data.ByteString.Lazy.Char8 as BL

data CFUploadResponse = CFUploadResponse
  {
    result   :: CFUploadResult,
    success  :: Bool,
    errors   :: [String],
    messages :: [String]
  } deriving (Show)

data CFUploadResult = CFUploadResult {
    id                :: String,
    filename          :: String,
    uploaded          :: String,
    requireSignedURLs :: Bool,
    variants          :: [String]
  } deriving  (Show)



getVariantsP :: BL.ByteString -> Either String [String]
getVariantsP input =
  case eitherDecode input of
     Left err ->  Left err
     Right value -> parseEither variantsP value

--  GHCi > parseMaybe variantsP cfData
-- Just ["foobar@yada.io/bird.jpg"]
variantsP :: Value -> Parser [String]
variantsP = withObject "CFUploadResponse" $ \obj -> do
    variants <- obj .: "result" .-> "variants"
    return variants

(.->) :: FromJSON a => Parser Object -> Text -> Parser a
(.->) parser key = do
  obj <- parser
  obj .: key

instance FromJSON CFUploadResponse where
     parseJSON (Object v) =
        CFUploadResponse     <$>
            v .: "result"    <*> 
            v .: "success"   <*>
            v .: "errors"    <*>
            v .: "messages"

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



-- REFERENCES
-- https://williamyaoh.com/posts/2019-10-19-a-cheatsheet-to-json-handling.html << THE MOST HELPFUL
-- https://www.fpcomplete.com/haskell/library/aeson/


