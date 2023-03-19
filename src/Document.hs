{-# LANGUAGE DeriveGeneric #-}
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
import GHC.Generics

-- Define the Article constructor
-- e.g. Article 12 "some title" "some body text"
data Document = Document Text Text [ImageElement] [Text]-- id content imageElements packageList
     deriving (Show, Generic)


data ImageElement = ImageElement
      {
        url        :: String
      , filename   :: String
      } deriving (Show, Generic)

-- Tell Aeson how to create a Document object from JSON string.
instance FromJSON Document
 

-- Tell Aeson how to convert a Document object to a JSON string.
instance ToJSON Document

instance FromJSON ImageElement

instance ToJSON ImageElement

docId :: Document -> Text
docId (Document id _ _ _) = id

content :: Document -> Text
content (Document _ content _ _ ) = content

urlList :: Document -> [ImageElement]
urlList (Document _  _ urlList _) = urlList

packageList :: Document -> [Text]
packageList (Document _  _ _ packageList) = packageList

packagePaths doc = 
  joinStrings " " $ fmap (\s -> "package/" ++ s) $ fmap unpack $ packageList $ doc

fixGraphicsPath = replace "\\graphicspath{ {image/} }" "\\graphicspath{{inbox/tmp/image/}}"

writeTeXSourceFile :: Document -> IO()
writeTeXSourceFile doc = 
  let
    docId' = unpack $ docId doc 
    texFilename = "inbox/" ++ docId'
    contents = unpack $ content doc
  in
    writeFile texFilename contents

downloadImage :: ImageElement -> IO (GHC.IO.Exception.ExitCode)
downloadImage element =
   system ("wget -O " ++ "image/" ++ filename element ++ " " ++  url element)

prepareData :: Document -> IO()
prepareData doc =
  let
      preparePackages = "cp " ++ packagePaths doc ++ " inbox/"
  in
    do
      mapM_ downloadImage (urlList doc) -- write the image files to ./image
      system preparePackages            -- copy any packages needed from ./package to ./inbox
      writeTeXSourceFile doc            -- write the tex file to ./inbox
     

joinStrings :: String -> [String] -> String
joinStrings separator [] = ""
joinStrings separator [x] = x
joinStrings separator (x:xs) = x ++ separator ++ joinStrings separator xs
