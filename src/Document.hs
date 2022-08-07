{-# LANGUAGE OverloadedStrings #-}

module Document (Document, fixGraphicsPath, docId, writeTeXSourceFile, prepareData, cleanImages) where
import Data.Text.Lazy ( unpack, Text )
import Data.Text.Lazy.Encoding
import Data.Aeson
import Control.Applicative
import System.Process
import Data.List.Split
import Data.List.Utils (replace)

-- Define the Article constructor
-- e.g. Article 12 "some title" "some body text"
data Document = Document Text Text [Text] -- id content imageUrls
     deriving (Show)


docId :: Document -> Text
docId (Document id _ _) = id

content :: Document -> Text
content (Document _ content _ ) = content

urlList :: Document -> [Text]
urlList (Document _  _ urlList ) = urlList

-- Tell Aeson how to create a Document object from JSON string.
instance FromJSON Document where
     parseJSON (Object v) = Document <$>
                            v .: "id" <*> 
                            v .:  "content" <*>
                            v .: "urlList"
 

-- Tell Aeson how to convert a Document object to a JSON string.
instance ToJSON Document where
     toJSON (Document id content imageUrls) =
         object ["id" .= id,
                 "content" .= content,
                 "urlList" .= imageUrls]


fixGraphicsPath = replace "\\graphicspath{ {image/} }" "\\graphicspath{{inbox/tmp/image/}}"

writeTeXSourceFile :: Document -> IO()
writeTeXSourceFile doc = 
  let
    texFile = "inbox/" ++ (unpack $ docId doc) 
    contents = fixGraphicsPath $ unpack $ content doc
  in
    writeFile texFile contents

writeTeXSourceFileTmp :: Document -> IO()
writeTeXSourceFileTmp doc = 
  let
    texFile = "inbox/tmp/" ++ (unpack $ docId doc) 
    contents = unpack $ content doc
  in
    writeFile texFile contents

cleanImages :: Text -> IO()
cleanImages docId =
     do
       let  manifestimageManifest = "inbox/" ++ (unpack docId) ++ "_image_manifest.txt"
       manifest <- readFile manifestimageManifest 
       let commands = Document.removeImagesCommand manifest  
       system commands >>= \exitCode -> print exitCode  



prepareData :: Document -> IO()
prepareData doc =
  let
      urlData =  joinStrings "\n" $ Prelude.map unpack  (urlList doc)
      imageManifest = "inbox/tmp/" ++ (unpack $ docId doc) ++ "_image_manifest.txt"
      -- imageDirectory1 = "image/" ++ (unpack $ docId doc) ++ ""
      imageDirectory = "inbox/tmp/image/"
      -- cmd = "wget -P image -i " ++ imageManifest
      -- make document with normal image urls
      getNormalImageimageManifests = "grep -v image.png " ++ imageManifest ++ " > "  ++  (imageManifest ++ "-1")
      -- make document with image urls for ibb.co
      getIBBImageimageManifests = "grep image.png " ++ imageManifest ++ " > " ++ (imageManifest ++ "-2")
      -- get the normal images
      getNormalImages = "wget -P " ++ imageDirectory ++ " -i " ++ (imageManifest ++ "-1")
      -- get the ibb.co images
      getIBBmages = "wget -P " ++ imageDirectory ++ " -i " ++ (imageManifest ++ "-2") ++ " -x"
      l1 = "for p in `cat " ++ (imageManifest ++ "-2") ++ " | sed 's/https:\\/\\/i.ibb.co\\///g' | sed 's/\\/image.png//g'`\n"
      l2 = "do\n"
      l3 = "cp image/i.ibb.co/$p/image.png " ++ imageDirectory ++ " $p.png\n"
      l3b = "cp image/i.ibb.co/$p/image.jpb " ++ imageDirectory ++ " $p.jpg\n"
      l4 = "done"
      copyFilesFromIBBDirToImageDirectory = l1 ++ l2 ++ l3 ++ l4
      cleanup = "rm inbox/tmp/*; rm inbox/tmp/image/*"
      cleanManifests = "rm inbox/tmp/*_manifest.txt*"
  in
    do 
      system cleanup  >>= \exitCode -> print exitCode
      writeTeXSourceFile doc
      writeTeXSourceFileTmp doc
      writeFile imageManifest urlData
      system getNormalImageimageManifests >>= \exitCode -> print exitCode
      system getIBBImageimageManifests >>= \exitCode -> print exitCode
      system getNormalImages >>= \exitCode -> print exitCode
      system getIBBmages >>= \exitCode -> print exitCode
      system cleanManifests  >>= \exitCode -> print exitCode
      system copyFilesFromIBBDirToImageDirectory >>= \exitCode -> print exitCode
      

removeImagesCommand :: String -> String
removeImagesCommand manifest =
    joinStrings "; " $ map removeImageCommand $ lines manifest  


removeImageCommand :: String -> String
removeImageCommand imageName = 
    "rm image/" ++ (getImageName imageName)

getImageName1 :: String -> String
getImageName1 str = last $ splitOn "/" str

getImageName :: String -> String
getImageName str =
  case reverse $ splitOn "/" str of
    ("image.png":url:rest) -> url ++ ".png"
    (last:rest) -> last
    _ -> "nothing.png"



joinStrings :: String -> [String] -> String
joinStrings separator [] = ""
joinStrings separator [x] = x
joinStrings separator (x:xs) = x ++ separator ++ (joinStrings separator xs) 
