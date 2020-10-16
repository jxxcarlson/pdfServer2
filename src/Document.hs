{-# LANGUAGE OverloadedStrings #-}

module Document (Document, docId, write, writeImageManifest) where
import Data.Text.Lazy ( unpack, Text )
import Data.Text.Lazy.Encoding
import Data.Aeson
import Control.Applicative
import System.Process

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

writeImageManifest :: Document -> IO()
writeImageManifest doc =
  let
    urlData =  joinStrings "\n" $ Prelude.map unpack  (urlList doc)
    fileName = "texFiles/" ++ (unpack $ docId doc) ++ "_image_manifest.txt"
    -- cmd = "wget -i -P " ++ "texFiles/image " ++ fileName
    cmd = "wget -P image -i " ++ fileName

  in 
    do 
      writeFile fileName urlData
      system cmd >>= \exitCode -> print exitCode





joinStrings :: String -> [String] -> String
joinStrings separator [] = ""
joinStrings separator [x] = x
joinStrings separator (x:xs) = x ++ separator ++ (joinStrings separator xs) 
