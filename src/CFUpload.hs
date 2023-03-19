{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module CFUpload where


import Data.Aeson
import GHC.Generics
import Data.Text

data CFUploadResponse = CFUploadResponse
  {
    -- result   :: CFUploadResult,
    success  :: Bool,
    errors   :: [String],
    messages :: [String]
  } deriving (Generic, Show)

-- data CFUploadResult = CFUploadResult {
--     id                :: String,
--     filename          :: String,
--     uploaded          :: String,
--     requireSignedURLs :: Bool,
--     variants          :: [String]
--   } deriving  (Generic, Show)

instance FromJSON CFUploadResponse 

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