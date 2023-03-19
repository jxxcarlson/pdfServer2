{-# LANGUAGE OverloadedStrings #-}


-- https://dev.to/parambirs/how-to-write-a-haskell-web-servicefrom-scratch---part-3-5en6
-- https://adit.io/posts/2013-04-15-making-a-website-with-haskell.html

module Main where

import Control.Monad.IO.Class (liftIO) -- liftIO :: IO a -> m a
import Network.HTTP.Types
import Network.Wai.Middleware.Static ( (>->), addBase, noDots, staticPolicy )
import Web.Scotty
import Network.Wai.Middleware.Cors
import Network.Wai                       (Application, Middleware)
import Network.Wai.Middleware.AddHeaders (addHeaders)
import Network.Wai.Middleware.RequestLogger
import Data.Aeson
import System.Process
import Data.Text.Lazy (pack, unpack, replace, toLower, Text)


import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.List.Utils as U

import Document (Document, writeTeXSourceFile, prepareData, docId)
import qualified Image 
import qualified CFImage       
import qualified CFUpload     
import qualified CFOnetimeUrl  
import Pdf
import Tar

main = scotty 3000 $ do
 
    middleware defaultMiddlewares
    middleware logStdoutDev 

    post "/image" $ do
        image <- jsonData :: ActionM CFImage.CFImage  -- (1)
        let filename = CFImage.getFilenameFromImage image    -- (2)

        liftIO $ CFImage.downloadImage image -- (3)
        cfImageUploadUrl <- liftIO Image.requestCFToken -- (4)
        -- OK to here :::: text $ pack cfImageUploadUrl :::: cfImageUploadUrl = "https://upload.imagedelivery.net/9U-0Y4sEzXlO6BXzTnQnYQ/4015d17d-7732-4e45-e87e-6c07ff6f4b00"
        
        cfUploadedImageResponse <- liftIO $ Image.uploadTheImage cfImageUploadUrl filename -- (5)
        -- OK to here :::: text $ pack cfUploadedImageResponse  -- ::: "{\n  \"result\": {\n    \"id\": \"085ddcc3-988a-40b1-9f73-62f24d965700\",\n    \"filename\": \"bird2.jpg\",\n    \"uploaded\": \"2023-03-19T14:44:12.579Z\",\n    \"requireSignedURLs\": false,\n    \"variants\": [\n      \"https://imagedelivery.net/9U-0Y4sEzXlO6BXzTnQnYQ/085ddcc3-988a-40b1-9f73-62f24d965700/public\"\n    ]\n  },\n  \"success\": true,\n  \"errors\": [],\n  \"messages\": []\n}"
       
        let cfUploadedImageResponse' = eitherDecode $ BL.pack cfUploadedImageResponse :: Either String (Maybe CFUpload.CFUploadResponse)  -- (6)
        case cfUploadedImageResponse' of 
            Left err -> text $ pack err                     -- (7)  Error in $: parsing CFUpload.CFUploadResponse(CFUploadResponse) failed, expected Object, but encountered String
            Right goodStuff-> text $ pack $ show goodStuff  -- (8)

    post "/pdf" $ do
        document <- jsonData :: ActionM Document
        liftIO $ Document.prepareData document
        liftIO $ Pdf.create document
        text  (textReplace ".tex" ".pdf" (Data.Text.Lazy.toLower (Document.docId document)))

    post "/tar" $ do
        document <- jsonData :: ActionM Document 
        liftIO $ Document.prepareData document
        liftIO $ Tar.create document
        text (Document.docId document)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   

    get "/pdf/:id" $ do
        docId <- param "id"
        file ("outbox/" ++ docId)


    get "/tar/:id" $ do
        docId <- param "id"
        file ("outbox/" ++ unpack docId )

    get "/hello" $ do
        html $ mconcat ["Yes, I am still here\n"]

    post "/hello" $ do
       text "Yes, I am still here\n"

    middleware $ staticPolicy (noDots >-> addBase "outbox")

textReplace :: String -> String -> Text -> Text
textReplace src target text = 
    replace (pack src) (pack target) text

defaultMiddlewares :: Network.Wai.Application -> Network.Wai.Application
-- defaultMiddlewares = compression . staticFiles "public" . allowCsrf . corsified  
defaultMiddlewares =   allowCsrf . corsified  


-- | @x-csrf-token@ allowance.
-- The following header will be set: @Access-Control-Allow-Headers: x-csrf-token@.
allowCsrf :: Middleware
allowCsrf = addHeaders [("Access-Control-Allow-Headers", "x-csrf-token,authorization")]


-- | CORS middleware configured with 'appCorsResourcePolicy'.
corsified :: Middleware
corsified = cors (const $ Just appCorsResourcePolicy)    

-- | CORS middleware configured with 'appCorsResourcePolicy'.
appCorsResourcePolicy :: CorsResourcePolicy
appCorsResourcePolicy = CorsResourcePolicy
  { corsOrigins        = Nothing
  , corsMethods        = ["OPTIONS", "GET", "PUT", "POST", "DELETE"]
  , corsRequestHeaders = ["Authorization", "Content-Type", "Origin"]
  , corsExposedHeaders = Nothing
  , corsMaxAge         = Nothing
  , corsVaryOrigin     = False
  , corsRequireOrigin  = False
  , corsIgnoreFailures = False
  }

-- | Convert BL.ByteString to Text
blToText :: BL.ByteString -> Text
blToText = TLE.decodeUtf8