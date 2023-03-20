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


(.->) :: FromJSON a => Parser Object -> Text -> Parser a
(.->) parser key = do
  obj <- parser
  obj .: key

cfData :: Value
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


--  GHCi > parseMaybe variantsP cfData
-- Just ["foobar@yada.io/bird.jpg"]
variantsP :: Value -> Parser [String]
variantsP = withObject "CFUploadResponse" $ \obj -> do
    variants <- obj .: "result" .-> "variants"
    return variants


testInput :: String
testInput = "{\n  \"result\": {\n    \"id\": \"d637143d-5d4f-4f03-5238-f352c96e0800\",\n    \"filename\": \"bird2.jpg\",\n    \"uploaded\": \"2023-03-20T03:57:30.745Z\",\n    \"requireSignedURLs\": false,\n    \"variants\": [\n      \"https://imagedelivery.net/9U-0Y4sEzXlO6BXzTnQnYQ/d637143d-5d4f-4f03-5238-f352c96e0800/public\"\n    ]\n  },\n  \"success\": true,\n  \"errors\": [],\n  \"messages\": []\n}"

--- hohoho = eitheDecode testInput

--  GHCi > getVariantsP $ BL.pack testInput
--  Right ["https://imagedelivery.net/9U-0Y4sEzXlO6BXzTnQnYQ/d637143d-5d4f-4f03-5238-f352c96e0800/public"]
-- eitherDecode :: FromJSON a => BL.ByteString -> Either String a
getVariantsP :: BL.ByteString -> Either String [String]
getVariantsP input =
  case eitherDecode input of
     Left err ->  Left err
     Right value -> parseEither variantsP value


--  GHCi > getVariantsQ testInput
--  Right "https://imagedelivery.net/9U-0Y4sEzXlO6BXzTnQnYQ/d637143d-5d4f-4f03-5238-f352c96e0800/public"
--  GHCi > getVariantsQ (";lk;kj" ++ testInput ++ "L:KJ:LKJ")
--  Left "Error in $: Failed reading: not a valid json value at ';lk;kj{'"
getVariantsQ :: String -> Either String String
getVariantsQ input =
  case eitherDecode (BL.pack input) of
     Left err ->  Left err
     Right value -> Data.List.intercalate ", " <$> parseEither variantsP value




-- getVariantsPP :: BL.ByteString -> Either String String
-- getVariantsPP input =
--   case eitherDecode input of
--      Left err ->  Left err
--      Right value ->  
--        case parseEither variantsP value of
--           Left err2 -> err2
--           Right stuff -> Data.List.intercalate ", " stuff


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