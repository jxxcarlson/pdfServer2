{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module CFUpload where


import Data.Aeson
import Data.Aeson.Types
import GHC.Generics
import Data.Text
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

cfData = object [
    "result" .= object  [ 
        "id" .= ("abc" :: String)
        , "filename" .= ("foo.jpg" :: String)
        , "uploaded" .= ("2023-03-19" :: String)
        , "requireSignedUrls" .= (False ::Bool)
        ,  "variants" .= (["foobar@yada.io/bird.jpg"] :: [String])
    ]
    , "success" .= (True :: Bool)
    , "errors" .= ([]:: [String])
    , "messages" .= ([]::  [String])
   ]


foo = object
    [ "contact_info" .= object
      [ "email" .= ("williamyaoh@gmail.com" :: String)
      , "address" .= object
        [ "state" .= ("OK" :: String)
        , "zip_code" .= ("74008" :: String)
        ]
      ]
   ]


email = parseMaybe nested $ object
    [ "contact_info" .=
      object [ "email" .= ("williamyaoh@gmail.com" :: String) ]
    ]  



(.->) :: FromJSON a => Parser Object -> Text -> Parser a
(.->) parser key = do
  obj <- parser
  obj .: key


--  GHCi > parseMaybe CFUpload.nested' foo
-- Just ("williamyaoh@gmail.com","OK")  
nested' :: Value -> Parser (String, String)
nested' = withObject "ContactInfo" $ \obj -> do
   email <- obj .: "contact_info" .-> "email"
   state <- obj .: "contact_info" .-> "address" .-> "state"
   return (email, state)

-- { contact_info: { email: <string> } }
nested :: Value -> Parser String
nested = withObject "ContactInfo" $ \obj -> do
  contact <- obj .: "contact_info"
  contact .: "email"

--  GHCi > parseMaybe variantsP cfData
-- Just ["foobar@yada.io/bird.jpg"]
variantsP :: Value -> Parser [String]
variantsP = withObject "CFUploadResult" $ \obj -> do
    variants <- obj .: "result" .-> "variants"
    return variants
    
-- getVariantsP :: BL.ByteString -> Either String [String]

-- getVariantsPP input = do
--   object <- eitherDecode input
--   variantsP object


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
      


testResponse = "{\n  \"result\": {\n    \"id\": \"49660d63-a43f-4011-1a7a-ff6435305d00\",\n    \"filename\": \"bird2.jpg\",\n    \"uploaded\": \"2023-03-16T23:08:22.768Z\",\n    \"requireSignedURLs\": false,\n    \"variants\": [\n      \"https://imagedelivery.net/9U-0Y4sEzXlO6BXzTnQnYQ/49660d63-a43f-4011-1a7a-ff6435305d00/public\"\n    ]\n  },\n  \"success\": true,\n  \"errors\": [],\n  \"messages\": []\n}"

testDecode str = Data.Aeson.eitherDecode $ BL.pack str :: Either String (Maybe CFUpload.CFUploadResponse)

myDecode str = Data.Aeson.decode $ BL.pack str :: Maybe CFUpload.CFUploadResponse 


uploadResult :: CFUploadResponse -> CFUploadResult
uploadResult (CFUploadResponse result _ _ _) = result

getVariants :: CFUploadResult -> [String]
getVariants (CFUploadResult _ _ _ _ variants)  = variants

getUploadUrlFromResponse :: CFUploadResponse -> [String]
getUploadUrlFromResponse = getVariants . uploadResult



-- REFERENCES
-- https://www.fpcomplete.com/haskell/library/aeson/



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