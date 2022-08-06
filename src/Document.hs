{-# LANGUAGE OverloadedStrings #-}

module Document (Document, docId, write, writeImageManifest, makeTarFile, cleanImages) where
import Data.Text.Lazy ( unpack, Text )
import Data.Text.Lazy.Encoding
import Data.Aeson
import Control.Applicative
import System.Process
import Data.List.Split

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


write :: Document -> IO()
write doc = 
  let
    fileName = "texFiles/" ++ (unpack $ docId doc) ++ ".tex"
    contents = unpack $ content doc
  in
    writeFile fileName contents

cleanImages :: Text -> IO()
cleanImages docId =
     do
       let  manifestFileName = "texFiles/" ++ (unpack docId) ++ "_image_manifest.txt"
       manifest <- readFile manifestFileName 
       let commands = Document.removeImagesCommand manifest  
       system commands >>= \exitCode -> print exitCode  


writeImageManifest :: Document -> IO()
writeImageManifest doc =
  let
    urlData =  joinStrings "\n" $ Prelude.map unpack  (urlList doc)
    fileName = "texFiles/" ++ (unpack $ docId doc) ++ "_image_manifest.txt"
    -- cmd = "wget -P image -i " ++ fileName
    -- make document with normal image urls
    cmd1 = "grep -v image.png " ++ fileName ++ " > "  ++  (fileName ++ "-1")
    -- make documet with image urls for ibb.co
    cmd2 = "grep image.png " ++ fileName ++ " > " ++ (fileName ++ "-2")
    -- get the normal images
    cmd3 = "wget -P image -i " ++ (fileName ++ "-1")
    -- get the ibb.co images
    cmd4 = "wget -P image -i " ++ (fileName ++ "-2") ++ " -x"
    l1 = "for p in `cat " ++ (fileName ++ "-2") ++ " | sed 's/https:\\/\\/i.ibb.co\\///g' | sed 's/\\/image.png//g'`\n"
    l2 = "do\n"
    l3 = "cp image/i.ibb.co/$p/image.png image/$p.png\n"
    l4 = "done"
    cmd5 = l1 ++ l2 ++ l3 ++ l4
  in
    do 
      writeFile fileName urlData
      system cmd1 >>= \exitCode -> print exitCode
      system cmd2 >>= \exitCode -> print exitCode
      system cmd3 >>= \exitCode -> print exitCode
      system cmd4 >>= \exitCode -> print exitCode
      system cmd5 >>= \exitCode -> print exitCode


-- wget -Px bar -i foo.txt
-- wget -P bar -i foo.txt -x

-- foo.txt:
-- https://psurl.s3.amazonaws.com/images/jc/sinc2-bcbf.png
-- https://psurl.s3.amazonaws.com/images/jc/beats-eca1.png
-- https://pentucketnews.com/wp-content/uploads/2014/11/Classic.jpg
-- https://i.ibb.co/T0wS1CD/image.png
-- https://i.ibb.co/Fs0xQtq/image.png

-- cp bar/i.ibb.co/T0wS1CD/image.png bar/T0wS1CD.png
-- 1. grep -v image.png foo.txt > foo.txt-1
-- 2. grep image.png foo.txt > foo.txt-2
-- 3. wget -P bar -i foo.txt-1
-- 4. wget -P bar -i foo.txt-2 -x


makeTarFile :: Document -> IO()
makeTarFile doc =
  let
    urlData =  joinStrings "\n" $ Prelude.map unpack  (urlList doc)
    fileName = "texFiles/" ++ (unpack $ docId doc) ++ "_image_manifest.txt"
    imageDirectory = "image/" ++ (unpack $ docId doc) ++ "/"
    -- cmd = "wget -P image -i " ++ fileName
    -- make document with normal image urls
    cmd1 = "grep -v image.png " ++ fileName ++ " > "  ++  (fileName ++ "-1")
    -- make document with image urls for ibb.co
    cmd2 = "grep image.png " ++ fileName ++ " > " ++ (fileName ++ "-2")
    -- get the normal images
    cmd3 = "wget -P " ++ imageDirectory ++ " -i " ++ (fileName ++ "-1")
    -- get the ibb.co images
    cmd4 = "wget -P " ++ imageDirectory ++ " -i " ++ (fileName ++ "-2") ++ " -x"
    l1 = "for p in `cat " ++ (fileName ++ "-2") ++ " | sed 's/https:\\/\\/i.ibb.co\\///g' | sed 's/\\/image.png//g'`\n"
    l2 = "do\n"
    l3 = "cp image/i.ibb.co/$p/image.png " ++ imageDirectory ++ " $p.png\n"
    l4 = "done"
    cmd5 = l1 ++ l2 ++ l3 ++ l4
  in
    do 
      writeFile fileName urlData
      system cmd1 >>= \exitCode -> print exitCode
      system cmd2 >>= \exitCode -> print exitCode
      system cmd3 >>= \exitCode -> print exitCode
      system cmd4 >>= \exitCode -> print exitCode
      system cmd5 >>= \exitCode -> print exitCode

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
