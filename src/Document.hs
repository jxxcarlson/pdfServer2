{-# LANGUAGE OverloadedStrings #-}

module Document (Document, fixGraphicsPath, docId, writeTeXSourceFile, prepareData) where
import Data.Text.Lazy ( unpack, Text )
import Data.Text.Lazy.Encoding
import Data.Aeson
import Control.Applicative
import System.Process
import Data.List.Split
import Data.List.Utils (replace)
import GHC.IO.Exception

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

writeTeXSourceFile :: Document -> IO()
writeTeXSourceFile doc = 
  let
    texFilename = "inbox/" ++ (unpack $ docId doc) 
    contents = fixGraphicsPath $ unpack $ content doc
  in
    writeFile texFilename contents

writeTeXSourceFileTmp :: Document -> IO()
writeTeXSourceFileTmp doc = 
  let
    texFilename = "inbox/tmp/" ++ (unpack $ docId doc) 
    contents = unpack $ content doc
  in
    writeFile texFilename contents

imageDirectory = "inbox/tmp/image/"

downloadImage :: ImageElement -> IO (GHC.IO.Exception.ExitCode)
downloadImage element =
   system ("wget -O " ++ imageDirectory ++ (filename element) ++ " " ++  (url element))


prepareData :: Document -> IO()
prepareData doc =
  let
      preparePackages = "cp " ++ (packagePaths doc) ++ " inbox/tmp/"
      cleanup = "rm inbox/tmp/*; rm inbox/tmp/image/*"
      cleanManifests = "rm inbox/tmp/*_manifest.txt*"
  in
    do
      system cleanup  >>= \exitCode -> print exitCode
      writeTeXSourceFile doc
      writeTeXSourceFileTmp doc
      system preparePackages
      mapM_ downloadImage (urlList doc)
      

joinStrings :: String -> [String] -> String
joinStrings separator [] = ""
joinStrings separator [x] = x
joinStrings separator (x:xs) = x ++ separator ++ (joinStrings separator xs) 
