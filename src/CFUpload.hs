{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module CFUpload where


import Data.Aeson
import GHC.Generics
import Data.Text
import qualified Data.ByteString.Lazy.Char8 as BL

data CFUploadResponse = CFUploadResponse
  {
    result   :: CFUploadResult,
    success  :: Bool,
    errors   :: [String],
    messages :: [String]
  } deriving (Generic, Show)

data CFUploadResult = CFUploadResult {
    id                :: String,
    filename          :: String,
    uploaded          :: String,
    requireSignedURLs :: Bool,
    variants          :: [String]
  } deriving  (Generic, Show)

instance FromJSON CFUploadResponse

instance FromJSON CFUploadResult

testResponse = "{\n  \"result\": {\n    \"id\": \"49660d63-a43f-4011-1a7a-ff6435305d00\",\n    \"filename\": \"bird2.jpg\",\n    \"uploaded\": \"2023-03-16T23:08:22.768Z\",\n    \"requireSignedURLs\": false,\n    \"variants\": [\n      \"https://imagedelivery.net/9U-0Y4sEzXlO6BXzTnQnYQ/49660d63-a43f-4011-1a7a-ff6435305d00/public\"\n    ]\n  },\n  \"success\": true,\n  \"errors\": [],\n  \"messages\": []\n}"

testDecode str = Data.Aeson.eitherDecode $ BL.pack str :: Either String (Maybe CFUpload.CFUploadResponse)

myDecode str = Data.Aeson.decode $ BL.pack str :: Maybe CFUpload.CFUploadResponse 


-- instance FromJSON CFUploadResult 
  

-- uploadResult :: CFUploadResponse -> CFUploadResult
-- uploadResult (CFUploadResponse result _ _ _) = result

-- getVariants :: CFUploadResult -> [String]
-- getVariants (CFUploadResult _ _ _ _ variants)  = variants

-- getUploadUrlFromResponse :: CFUploadResponse -> [String]
-- getUploadUrlFromResponse = getVariants . uploadResult



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