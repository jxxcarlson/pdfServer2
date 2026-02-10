{-# LANGUAGE OverloadedStrings #-}


-- https://dev.to/parambirs/how-to-write-a-haskell-web-servicefrom-scratch---part-3-5en6
-- https://adit.io/posts/2013-04-15-making-a-website-with-haskell.html

module Main where

import Control.Monad.IO.Class (liftIO) -- liftIO :: IO a -> m a
import Web.Scotty
import Network.HTTP.Types
import Network.Wai.Middleware.Static ( (>->), addBase, noDots, staticPolicy )
import Web.Scotty
import Network.Wai.Middleware.Cors
import Network.Wai                       (Application, Middleware)
import Network.Wai.Middleware.AddHeaders (addHeaders)
import Network.Wai.Middleware.RequestLogger
import Data.Aeson (encode, object, (.=))
import System.Process
import Data.Text.Lazy (pack, unpack, replace, toLower, Text)
import Pdf (create, createWithErrorPdf, createWithFailedImages, createWithFilteredErrors, PdfResult(..))
import Tar
import Document (Document, prepareData, docId)
import Image (CFImage,prepareCFImage,requestCFToken, updateCFImage, uploadTheImage, getFilenameFromImage)
import Tikz (TikzRequest, TikzResponse(..), convertTikzToPng)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.List.Utils as U

main = scotty 3000 $ do
 
    middleware defaultMiddlewares
    middleware logStdoutDev 

    post "/tikz2png" $ do
        tikzReq <- jsonData :: ActionM TikzRequest
        result <- liftIO $ convertTikzToPng tikzReq
        case result of
            TikzSuccess _ _ -> do
                status status200
                json result
            TikzError _ _ -> do
                status status400
                json result

    post "/image" $ do
        image <- jsonData :: ActionM CFImage
        liftIO $ prepareCFImage image
        cfImageUploadUrl <- liftIO $ requestCFToken
        let cfImageUploadUrl' = U.replace "\"" "" cfImageUploadUrl
        let filename = getFilenameFromImage image
        -- OK TO HERE:
        -- text $ pack cfImageUploadUrl'
        cfUploadedImageResponse <- liftIO $ uploadTheImage cfImageUploadUrl' filename
        -- text $ blToText $ encode updatedImage
        text $ pack cfUploadedImageResponse

    post "/json" $ do
        document <- jsonData :: ActionM Document
        failedImages <- liftIO $ Document.prepareData document
        result <- liftIO $ Pdf.create document
        case result of
            PdfSuccess fname -> do
                status status200
                json result
            PdfWithErrors _ _ _ _ -> do
                -- Return 200 OK but with all three files in response
                status status200
                json result
            PdfError _ _ -> do
                -- Generate filtered error PDF instead of returning raw log
                pdfFileName <- liftIO $ Pdf.createWithFilteredErrors document failedImages
                status status400
                json $ object ["pdf" .= (Nothing :: Maybe Text), "errorReport" .= pdfFileName, "hasErrors" .= True, "pdfFailed" .= True]

    post "/pdf" $ do
        document <- jsonData :: ActionM Document
        failedImages <- liftIO $ Document.prepareData document
        -- Get the full result to check if we have both PDFs
        result <- liftIO $ Pdf.create document
        case result of
            PdfSuccess fname ->
                -- Return JSON for consistency
                json $ object ["pdf" .= fname, "hasErrors" .= False]
            PdfWithErrors pdfFile errorPdfFile errorJsonFile _ -> do
                -- Return JSON with all three filenames when we have errors
                json $ object ["pdf" .= pdfFile, "errorReport" .= errorPdfFile, "errorJson" .= errorJsonFile, "hasErrors" .= True]
            PdfError _ _ -> do
                -- Generate error PDF for complete failures
                pdfFileName <- liftIO $ Pdf.createWithFilteredErrors document failedImages
                -- Return JSON indicating complete failure
                -- Include "pdf" as null so the frontend decoder succeeds
                json $ object ["pdf" .= (Nothing :: Maybe Text), "errorReport" .= pdfFileName, "hasErrors" .= True, "pdfFailed" .= True]

    post "/tar" $ do
        document <- jsonData :: ActionM Document
        failedImages <- liftIO $ Document.prepareData document
        liftIO $ Tar.create document
        text (Document.docId document)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   

    get "/pdf/:id" $ do
        docId <- param "id"
        file ("outbox/" ++ docId)


    get "/tar/:id" $ do
        docId <- param "id"
        file ("outbox/" ++ (unpack docId) )

    get "/hello" $ do
        html $ mconcat ["Yes, I am still here (c2f9a74)\n"]

    post "/hello" $ do
       text "Yes, I am still here\n"

    middleware $ staticPolicy (noDots >-> addBase "outbox")


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