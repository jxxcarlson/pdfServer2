{-# LANGUAGE OverloadedStrings #-}

module Tikz (TikzRequest, TikzResponse(..), convertTikzToPng) where

import Data.Text.Lazy (Text, pack, unpack)
import Data.Aeson
import Control.Applicative
import System.Process
import System.Exit (ExitCode(..))
import System.Directory (getCurrentDirectory, doesFileExist, copyFile)
import Control.Exception (catch, SomeException, try)
import System.FilePath ((</>))
import Image (requestCFToken, uploadTheImage)
import Data.List.Utils (replace)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BL

-- Request data type
data TikzRequest = TikzRequest
    { name     :: Text
    , content  :: Text
    , preamble :: Maybe Text
    } deriving Show

-- Response data type
data TikzResponse = TikzSuccess
    { responseName :: Text
    , url          :: Text
    }
    | TikzError
    { responseName :: Text
    , errorMsg     :: Text
    } deriving Show

-- FromJSON instance for TikzRequest
instance FromJSON TikzRequest where
    parseJSON = withObject "TikzRequest" $ \o -> do
        name <- o .: "name"
        content <- o .: "content"
        preamble <- o .:? "preamble"
        return $ TikzRequest name content preamble

-- ToJSON instance for TikzResponse
instance ToJSON TikzResponse where
    toJSON (TikzSuccess name url) =
        object ["name" .= name, "url" .= url]
    toJSON (TikzError name err) =
        object ["name" .= name, "error" .= err]

-- Cloudflare response types for parsing upload result
data CFImageUploadResult = CFImageUploadResult
    { cfId :: String
    , cfVariants :: [String]
    } deriving Show

data CFImageUploadResponse = CFImageUploadResponse
    { cfResult :: CFImageUploadResult
    , cfSuccess :: Bool
    } deriving Show

instance FromJSON CFImageUploadResult where
    parseJSON = withObject "CFImageUploadResult" $ \o -> do
        id <- o .: "id"
        variants <- o .: "variants"
        return $ CFImageUploadResult id variants

instance FromJSON CFImageUploadResponse where
    parseJSON = withObject "CFImageUploadResponse" $ \o -> do
        result <- o .: "result"
        success <- o .: "success"
        return $ CFImageUploadResponse result success

-- Convert TikZ code to PNG using the tikz2png.sh script and upload to Cloudflare
convertTikzToPng :: TikzRequest -> IO TikzResponse
convertTikzToPng (TikzRequest name tikzContent maybePreamble) = do
    let tikzCode = unpack tikzContent
        preambleCode = maybe "" unpack maybePreamble
        fileName = unpack name
        -- Generate output filename based on input name (replace .json with .png)
        outputName = if ".json" `elem` words fileName
                     then take (length fileName - 5) fileName ++ ".png"
                     else fileName ++ ".png"

    -- Get current directory to construct full paths
    pwd <- getCurrentDirectory
    let outputPath = pwd </> "outbox" </> outputName
        scriptPath = pwd </> "tikz2png" </> "tikz2png.sh"
        cfImagePath = pwd </> "cf-image" </> outputName
        -- Pass preamble as 4th argument if present
        scriptArgs = [tikzCode, outputPath, "300", preambleCode]

    -- Call tikz2png.sh script
    result <- try (readProcessWithExitCode scriptPath scriptArgs "") :: IO (Either SomeException (ExitCode, String, String))

    case result of
        Left ex ->
            return $ TikzError name (pack $ "Script execution failed: " ++ show ex)
        Right (ExitSuccess, stdout, _) -> do
            -- Check if the output file was created
            fileExists <- doesFileExist outputPath
            if fileExists
                then do
                    -- Upload to Cloudflare
                    uploadResult <- uploadToCloudflare outputPath cfImagePath outputName name
                    return uploadResult
                else return $ TikzError name (pack "PNG file was not created")
        Right (ExitFailure code, _, stderr) ->
            return $ TikzError name (pack $ "Conversion failed with exit code " ++ show code ++ ": " ++ stderr)

-- Upload PNG to Cloudflare and return the URL
uploadToCloudflare :: FilePath -> FilePath -> String -> Text -> IO TikzResponse
uploadToCloudflare outputPath cfImagePath outputName requestName = do
    -- Copy PNG to cf-image directory
    copyFile outputPath cfImagePath

    -- Get Cloudflare upload token
    cfImageUploadUrl <- requestCFToken
    let cfImageUploadUrl' = replace "\"" "" cfImageUploadUrl

    -- Upload the image
    cfUploadedImageResponse <- uploadTheImage cfImageUploadUrl' outputName

    -- Parse the response to extract the image URL
    -- Note: uploadTheImage returns show $ responseBody, so we need to read it back
    let unescapedResponse = read cfUploadedImageResponse :: String
        responseBS = BL.pack unescapedResponse
        maybeCFResponse = A.decode responseBS :: Maybe CFImageUploadResponse

    case maybeCFResponse of
        Nothing -> return $ TikzError requestName (pack $ "Failed to parse Cloudflare response: " ++ unescapedResponse)
        Just cfResponse ->
            if cfSuccess cfResponse && not (null (cfVariants (cfResult cfResponse)))
                then return $ TikzSuccess requestName (pack $ head (cfVariants (cfResult cfResponse)))
                else return $ TikzError requestName (pack $ "Cloudflare upload failed or no variants returned")