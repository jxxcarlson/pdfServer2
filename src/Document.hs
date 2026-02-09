{-# LANGUAGE OverloadedStrings #-}

module Document (Document, ImageElement(..), fixGraphicsPath, docId, content, prepareData, urlList) where
import Data.Text.Lazy ( unpack, Text, pack )
import Data.Text.Lazy.Encoding
import Data.Aeson
import Control.Applicative
import System.Process
import Data.List.Split
import Data.List.Utils (replace)
import Data.List (isInfixOf, isPrefixOf)
import GHC.IO.Exception
import System.Directory (getCurrentDirectory, doesFileExist, getFileSize)
import System.Exit (ExitCode(..))
import Control.Exception (catch, SomeException)
import Data.Char (isAlphaNum)

-- Define the Article constructor
-- e.g. Article 12 "some title" "some body text"
data Document = Document Text Text [ImageElement] [Text]-- id content imageElements packageList
     deriving (Show)


data ImageElement = ImageElement
      {
        url        :: String
      , filename   :: String
      } deriving Show

-- Tell Aeson how to create a Document object from JSON string.
instance FromJSON Document where
     parseJSON (Object v) = Document <$>
                            v .: "id" <*> 
                            v .: "content" <*>
                            v .: "urlList" <*>
                            v .: "packageList"
 

-- Tell Aeson how to convert a Document object to a JSON string.
instance ToJSON Document where
     toJSON (Document id content imageUrls packageList) =
         object ["id" .= id,
                 "content" .= content,
                 "urlList" .= imageUrls,
                 "packageList" .= packageList
                 ]


instance FromJSON ImageElement where
  parseJSON = withObject "ImageElement" $ \o -> do
    url <- o .: "url"
    filename <- o .: "filename"
    return $ ImageElement url filename

instance ToJSON ImageElement where
     toJSON (ImageElement url filename) =
         object ["id" .= url,
                 "filename" .= filename
                 ]


docId :: Document -> Text
docId (Document id _ _ _) = id

content :: Document -> Text
content (Document _ content _ _ ) = content

urlList :: Document -> [ImageElement]
urlList (Document _  _ urlList _) = urlList

packageList :: Document -> [Text]
packageList (Document _  _ _ packageList) = packageList

packagePaths doc = 
  (joinStrings " " $ fmap (\s -> "package/" ++ s) $ fmap unpack $ packageList $ doc)


fixGraphicsPath = replace "\\graphicspath{ {image/} }" "\\graphicspath{{inbox/tmp/image/}}"

writeTeXSourceFile :: Document -> [ImageElement] -> IO()
writeTeXSourceFile doc failedImages = do
    pwd <- getCurrentDirectory
    let texFilename = "inbox/" ++ (unpack $ docId doc)
        contents = unpack $ content doc
    -- Extract image URLs from \includegraphics commands, download them, and replace URLs with local filenames
    contentsWithLocalImages <- downloadAndReplaceImageUrls contents
    let -- Also replace any URLs from the urlList (if provided)
        contentsWithUrlList = replaceImageUrls contentsWithLocalImages (urlList doc)
        -- Add graphics path after documentclass if graphicx is used
        -- Use absolute path to ensure images are found
        contentsWithPath = if "\\usepackage{graphicx}" `isInfixOf` contentsWithUrlList
                           then replace "\\usepackage{graphicx}" ("\\usepackage{graphicx}\n\\graphicspath{{" ++ pwd ++ "/image/}}") contentsWithUrlList
                           else contentsWithUrlList
        -- Replace failed images with placeholder text
        contentsWithPlaceholders = replaceFailedImages contentsWithPath failedImages
    writeFile texFilename contentsWithPlaceholders

-- Extract http/https URLs from \includegraphics{...} commands in the content
extractImageUrls :: String -> [String]
extractImageUrls [] = []
extractImageUrls content =
    case breakOnIncludegraphics content of
        Nothing -> []
        Just rest ->
            -- rest starts just after the '{' of \includegraphics...{
            let (urlStr, remaining) = span (/= '}') rest
            in if "http" `isPrefixOf` urlStr
               then urlStr : extractImageUrls remaining
               else extractImageUrls remaining

-- Find the next \includegraphics{...} or \includegraphics[...]{...} and return the content after '{'
breakOnIncludegraphics :: String -> Maybe String
breakOnIncludegraphics [] = Nothing
breakOnIncludegraphics s@('\\':'i':'n':'c':'l':'u':'d':'e':'g':'r':'a':'p':'h':'i':'c':'s':rest) =
    case rest of
        ('{':after) -> Just after
        ('[':after) -> -- skip over options to find the '{'
            case dropWhile (/= ']') after of
                (']':'{':after2) -> Just after2
                _ -> breakOnIncludegraphics (drop 1 s)
        _ -> breakOnIncludegraphics (drop 1 s)
breakOnIncludegraphics (_:rest) = breakOnIncludegraphics rest

-- Generate a local filename from a URL (hash-like, using last path segments)
urlToFilename :: String -> Int -> String
urlToFilename url idx =
    let -- Take the last meaningful part of the URL path
        parts = splitOn "/" url
        meaningful = filter (not . null) parts
        namePart = if length meaningful >= 2
                   then meaningful !! (length meaningful - 2)  -- second to last segment
                   else "image"
        -- Sanitize: keep only alphanumeric and hyphens
        sanitized = filter (\c -> isAlphaNum c || c == '-') namePart
        safeName = if null sanitized then "image" else take 40 sanitized
    in safeName ++ "-" ++ show idx ++ ".png"

-- Download all image URLs found in \includegraphics commands and replace them with local filenames
downloadAndReplaceImageUrls :: String -> IO String
downloadAndReplaceImageUrls content = do
    let urls = extractImageUrls content
    if null urls then return content
    else do
        putStrLn $ "Found " ++ show (length urls) ++ " image URL(s) in \\includegraphics commands"
        -- Download each URL and build replacement list
        replacements <- mapM (\(idx, imageUrl) -> do
            let localName = urlToFilename imageUrl idx
            putStrLn $ "Downloading: " ++ imageUrl ++ " -> image/" ++ localName
            exitCode <- system ("curl -s -o image/" ++ localName ++ " \"" ++ imageUrl ++ "\"")
            case exitCode of
                ExitSuccess -> do
                    valid <- validateImage ("image/" ++ localName)
                    if valid then do
                        putStrLn $ "  OK: " ++ localName
                        return (imageUrl, localName)
                    else do
                        putStrLn $ "  WARN: Downloaded file too small, keeping URL"
                        return (imageUrl, imageUrl)  -- no replacement
                _ -> do
                    putStrLn $ "  WARN: Download failed, keeping URL"
                    return (imageUrl, imageUrl)  -- no replacement
            ) (zip [1..] urls)
        -- Apply all replacements
        let result = foldl (\c (old, new) -> replace old new c) content replacements
        return result

-- Replace each image URL in the content with its local filename
replaceImageUrls :: String -> [ImageElement] -> String
replaceImageUrls content [] = content
replaceImageUrls content (img:imgs) =
    let replaced = replace (url img) (filename img) content
    in replaceImageUrls replaced imgs

-- Replace references to failed images with a placeholder
replaceFailedImages :: String -> [ImageElement] -> String
replaceFailedImages content [] = content
replaceFailedImages content (img:imgs) =
    let placeholder = "\\fbox{\\parbox{0.8\\textwidth}{\\centering\\vspace{1em}\\textbf{Image Not Available}\\\\[0.5em]\\texttt{" ++ filename img ++ "}\\\\[0.5em]Could not download from:\\\\\\small\\texttt{" ++ take 50 (url img) ++ (if length (url img) > 50 then "..." else "") ++ "}\\vspace{1em}}}"
        -- Replace various forms of includegraphics commands that might reference this image
        patterns = [ "\\includegraphics[" ++ filename img ++ "]"
                   , "\\includegraphics{" ++ filename img ++ "}"
                   , "\\includegraphics{image/" ++ filename img ++ "}"
                   , "{" ++ filename img ++ "}"  -- Sometimes images are in figure environments
                   ]
        -- Look for includegraphics commands with this filename
        contentWithReplacement = replaceInGraphics content (filename img) placeholder
    in replaceFailedImages contentWithReplacement imgs

-- Helper to replace includegraphics commands for a specific filename
replaceInGraphics :: String -> String -> String -> String
replaceInGraphics content fname placeholder =
    -- Use a regex-like approach to find and replace all variations
    -- This is simpler and more reliable than trying to parse all possible LaTeX patterns
    let
        -- Replace basic forms
        content1 = replace ("\\includegraphics{" ++ fname ++ "}") placeholder content
        content2 = replace ("\\includegraphics{image/" ++ fname ++ "}") placeholder content1

        -- For patterns with options, we'll use a different approach
        -- Split by the filename and check if it's part of an includegraphics command
        content3 = replaceGraphicsWithOptions content2 fname placeholder
    in content3

-- Replace includegraphics commands that have options
replaceGraphicsWithOptions :: String -> String -> String -> String
replaceGraphicsWithOptions content fname placeholder =
    -- Check if the filename appears after \includegraphics
    if ("]{" ++ fname ++ "}") `isInfixOf` content
    then
        -- Find and replace patterns like \includegraphics[...]{fname}
        let parts = splitOn ("]{" ++ fname ++ "}") content
            processedParts = processIncludeParts parts placeholder fname False
        in joinStrings "" processedParts
    else if ("]{image/" ++ fname ++ "}") `isInfixOf` content
    then
        -- Find and replace patterns like \includegraphics[...]{image/fname}
        let parts = splitOn ("]{image/" ++ fname ++ "}") content
            processedParts = processIncludeParts parts placeholder fname True
        in joinStrings "" processedParts
    else content
  where
    processIncludeParts :: [String] -> String -> String -> Bool -> [String]
    processIncludeParts [] _ _ _ = []
    processIncludeParts [x] _ _ _ = [x]
    processIncludeParts (x:xs) repl fname hasImagePrefix =
        if "\\includegraphics[" `isInfixOf` x
        then
            -- Find the start of \includegraphics[ and replace everything from there
            let beforeGraphics = reverse $ drop 16 $ reverse x  -- Remove "\includegraphics["
            in (beforeGraphics ++ repl) : processIncludeParts xs repl fname hasImagePrefix
        else
            let suffix = if hasImagePrefix then "]{image/" ++ fname ++ "}" else "]{" ++ fname ++ "}"
            in x : suffix : processIncludeParts xs repl fname hasImagePrefix


-- Download image and return whether it was successful
downloadImage :: ImageElement -> IO Bool
downloadImage element = do
    exitCode <- system ("curl -s -o " ++ "image/" ++ (filename element) ++ " \"" ++  (url element) ++ "\"")
    case exitCode of
        ExitSuccess -> validateImage ("image/" ++ filename element)
        _ -> return False

-- Validate that an image file exists and is not too small
validateImage :: FilePath -> IO Bool
validateImage path = do
    exists <- doesFileExist path
    if exists then do
        size <- getFileSize path
        -- Check if file is larger than 100 bytes (likely not a valid image if smaller)
        return (size > 100)
    else
        return False

prepareData :: Document -> IO [ImageElement]
prepareData doc =
  let
      paths = packagePaths doc
      preparePackages = if null paths
                        then "true"  -- no-op command when no packages
                        else "cp " ++ paths ++ " inbox/"
  in
    do
      -- Download images and track which ones failed
      results <- mapM (\img -> do
          success <- downloadImage img
          if not success then
              putStrLn $ "Warning: Failed to download image: " ++ filename img ++ " from " ++ url img
          else
              putStrLn $ "Successfully downloaded: " ++ filename img
          return (img, success)) (urlList doc)

      let failedImages = map fst $ filter (not . snd) results

      -- Report summary with detailed URLs
      if not (null failedImages) then do
          putStrLn $ "\n========== IMAGE DOWNLOAD FAILURES =========="
          putStrLn $ "Failed to download " ++ show (length failedImages) ++ " images. Placeholders will be used."
          putStrLn "\nFailed image URLs:"
          mapM_ (\img -> putStrLn $ "  - " ++ filename img ++ ": " ++ url img) failedImages
          putStrLn "============================================\n"
      else
          putStrLn "All images downloaded successfully."

      system preparePackages            -- copy any packages needed from ./package to ./inbox
      writeTeXSourceFile doc failedImages  -- write the tex file to ./inbox with placeholders for failed images

      -- Return the list of failed images so they can be included in error reporting
      return failedImages
     
      

joinStrings :: String -> [String] -> String
joinStrings separator [] = ""
joinStrings separator [x] = x
joinStrings separator (x:xs) = x ++ separator ++ (joinStrings separator xs) 
