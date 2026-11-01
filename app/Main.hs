{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}


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
import Data.Aeson (encode, object, (.=), toJSON, Value)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import System.Process
import Data.Text.Lazy (pack, unpack, replace, toLower, Text)
import Pdf (create, createWithErrorPdf, createWithFailedImages, createWithFilteredErrors, PdfResult(..))
import Tar
import Document (Document, prepareData, docId, sanitizeDocument)
import qualified Cache
import Image (CFImage,prepareCFImage,requestCFToken, updateCFImage, uploadTheImage, getFilenameFromImage, sanitizeCFImage)
import Safe (isSafeName)
import System.Environment (setEnv, lookupEnv)
import System.Directory (getCurrentDirectory)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import GitHash (tGitInfoCwdTry, giHash, giDirty, giCommitDate)
import Tikz (TikzRequest, TikzResponse(..), convertTikzToPng)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.List.Utils as U

-- | The commit this program was built from, recorded at compile time
-- (Template Haskell runs git in the source tree). "-dirty" means the build
-- had uncommitted changes; "unknown" means it was built without a git
-- checkout.
version :: String
version = case $$tGitInfoCwdTry of
    Right gi -> take 7 (giHash gi) ++ (if giDirty gi then "-dirty" else "")
                ++ ", " ++ giCommitDate gi    -- e.g. "Sun Nov 1 21:40:12 2026 +0000"
    Left _ -> "unknown"

main :: IO ()
main = do
    putStrLn $ "pdfServer version " ++ version
    -- Lock down TeX for every compile this process starts (xelatex here,
    -- pdflatex in tikz2png.sh). Documents come from anyone on the internet:
    --   shell_escape=f   \write18 can't run programs
    --   openin_any=p     no dotfiles (.env), no "..", and absolute paths only
    --   openout_any=p    inside TEXMFOUTPUT, i.e. this folder
    -- TEXMFOUTPUT is the working directory because scripta's LaTeX uses an
    -- absolute \graphicspath into ./image.
    pwd <- getCurrentDirectory
    mapM_ (uncurry setEnv)
        [ ("shell_escape", "f"), ("openin_any", "p"), ("openout_any", "p")
        , ("TEXMFOUTPUT", pwd) ]
    port <- fromMaybe 3000 . (>>= readMaybe) <$> lookupEnv "PDFSERVER_PORT"
    -- One LaTeX job at a time. All jobs share inbox/, image/ and outbox/,
    -- and each one ends with `rm -f inbox/*.tex image/*`, so two at once
    -- deleted each other's files mid-compile. /tikz2png works in its own
    -- temporary folder and doesn't need the lock.
    lock <- newMVar ()
    server lock port

-- | Run a job's file work while holding the lock.
exclusively :: MVar () -> IO a -> ActionM a
exclusively lock job = liftIO (withMVar lock (const job))

server :: MVar () -> Int -> IO ()
server lock port = scotty port $ do
 
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
        image <- sanitizeCFImage <$> (jsonData :: ActionM CFImage)
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
        document <- sanitizeDocument <$> (jsonData :: ActionM Document)
        (code, body) <- exclusively lock $ do
            failedImages <- Document.prepareData document
            result <- Pdf.create document
            case result of
                PdfSuccess _ -> return (status200, toJSON result)
                -- Return 200 OK but with all three files in response
                PdfWithErrors _ _ _ _ -> return (status200, toJSON result)
                PdfError _ _ -> do
                    -- Generate filtered error PDF instead of returning raw log
                    pdfFileName <- Pdf.createWithFilteredErrors document failedImages
                    return (status400, object ["pdf" .= (Nothing :: Maybe Text), "errorReport" .= pdfFileName, "hasErrors" .= True, "pdfFailed" .= True])
        status code
        json body

    post "/pdf" $ do
        document <- sanitizeDocument <$> (jsonData :: ActionM Document)
        let hash = Cache.contentHash document
        let pdfName = unpack (replace ".tex" ".pdf" (docId document))
        body <- exclusively lock $ do
            hit <- Cache.cacheLookup hash ("outbox/" ++ pdfName)
            if hit
                then return $ object ["pdf" .= pdfName, "hasErrors" .= False]
                else do
                    Cache.pruneCache
                    failedImages <- Document.prepareData document
                    result <- Pdf.create document
                    case result of
                        PdfSuccess fname -> do
                            Cache.cacheStore hash ("outbox/" ++ unpack fname)
                            return $ object ["pdf" .= fname, "hasErrors" .= False]
                        PdfWithErrors pdfFile errorPdfFile errorJsonFile _ ->
                            return $ object ["pdf" .= pdfFile, "errorReport" .= errorPdfFile, "errorJson" .= errorJsonFile, "hasErrors" .= True]
                        PdfError _ _ -> do
                            pdfFileName <- Pdf.createWithFilteredErrors document failedImages
                            return $ object ["pdf" .= (Nothing :: Maybe Text), "errorReport" .= pdfFileName, "hasErrors" .= True, "pdfFailed" .= True]
        json body

    post "/tex" $ do
        document <- sanitizeDocument <$> (jsonData :: ActionM Document)
        body <- exclusively lock $ do
            failedImages <- Document.prepareData document
            let texFilePath = "inbox/" ++ unpack (Document.docId document)
            -- Read strictly: the file is deleted after the compile.
            texContent <- readFile texFilePath >>= \s -> length s `seq` return s
            result <- Pdf.create document
            case result of
                PdfSuccess fname ->
                    return $ object ["pdf" .= fname, "hasErrors" .= False, "tex" .= texContent]
                PdfWithErrors pdfFile errorPdfFile errorJsonFile _ ->
                    return $ object ["pdf" .= pdfFile, "errorReport" .= errorPdfFile, "errorJson" .= errorJsonFile, "hasErrors" .= True, "tex" .= texContent]
                PdfError _ _ -> do
                    pdfFileName <- Pdf.createWithFilteredErrors document failedImages
                    return $ object ["pdf" .= (Nothing :: Maybe Text), "errorReport" .= pdfFileName, "hasErrors" .= True, "pdfFailed" .= True, "tex" .= texContent]
        json body

    post "/tar" $ do
        document <- sanitizeDocument <$> (jsonData :: ActionM Document)
        exclusively lock $ do
            _ <- Document.prepareData document
            Tar.create document
        text (Document.docId document)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   

    get "/pdf/:id" $ do
        docId <- param "id"
        if isSafeName docId
            then file ("outbox/" ++ docId)
            else status status404


    get "/tar/:id" $ do
        docId <- param "id"
        if isSafeName (unpack docId)
            then file ("outbox/" ++ (unpack docId) )
            else status status404

    get "/hello" $ do
        html $ mconcat ["Yes, I am still here (", pack version, ")\n"]

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