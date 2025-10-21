{-# LANGUAGE OverloadedStrings #-}

module Tikz (TikzRequest, TikzResponse(..), convertTikzToPng) where

import Data.Text.Lazy (Text, pack, unpack)
import Data.Aeson
import Control.Applicative
import System.Process
import System.Exit (ExitCode(..))
import System.Directory (getCurrentDirectory, doesFileExist)
import Control.Exception (catch, SomeException, try)
import System.FilePath ((</>))

-- Request data type
data TikzRequest = TikzRequest
    { name    :: Text
    , content :: Text
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
        return $ TikzRequest name content

-- ToJSON instance for TikzResponse
instance ToJSON TikzResponse where
    toJSON (TikzSuccess name url) =
        object ["name" .= name, "url" .= url]
    toJSON (TikzError name err) =
        object ["name" .= name, "error" .= err]

-- Convert TikZ code to PNG using the tikz2png.sh script
convertTikzToPng :: TikzRequest -> IO TikzResponse
convertTikzToPng (TikzRequest name tikzContent) = do
    let tikzCode = unpack tikzContent
        fileName = unpack name
        -- Generate output filename based on input name (replace .json with .png)
        outputName = if ".json" `elem` words fileName
                     then take (length fileName - 5) fileName ++ ".png"
                     else fileName ++ ".png"

    -- Get current directory to construct full paths
    pwd <- getCurrentDirectory
    let outputPath = pwd </> "outbox" </> outputName
        scriptPath = pwd </> "tikz2png" </> "tikz2png.sh"

    -- Call tikz2png.sh script
    result <- try (readProcessWithExitCode scriptPath [tikzCode, outputPath, "300"] "") :: IO (Either SomeException (ExitCode, String, String))

    case result of
        Left ex ->
            return $ TikzError name (pack $ "Script execution failed: " ++ show ex)
        Right (ExitSuccess, stdout, _) -> do
            -- Check if the output file was created
            fileExists <- doesFileExist outputPath
            if fileExists
                then return $ TikzSuccess name (pack outputName)
                else return $ TikzError name (pack "PNG file was not created")
        Right (ExitFailure code, _, stderr) ->
            return $ TikzError name (pack $ "Conversion failed with exit code " ++ show code ++ ": " ++ stderr)