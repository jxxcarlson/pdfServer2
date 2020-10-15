{-# LANGUAGE OverloadedStrings #-}
module Document where
import Data.Text.Lazy
import Data.Text.Lazy.Encoding
import Data.Aeson
import Control.Applicative

-- Define the Article constructor
-- e.g. Article 12 "some title" "some body text"
data Document = Document Text Text Text -- id title content
     deriving (Show)


docId :: Document -> Text
docId (Document id _ _) = id

title :: Document -> Text
title (Document _ title _) = title

content :: Document -> Text
content (Document _ _ content) = content

-- Tell Aeson how to create a Document object from JSON string.
instance FromJSON Document where
     parseJSON (Object v) = Document <$>
                            v .: "id" <*> 
                            v .:  "title"    <*>
                            v .:  "content"


-- Tell Aeson how to convert a Document object to a JSON string.
instance ToJSON Document where
     toJSON (Document id title content) =
         object ["id" .= id,
                 "title" .= title,
                 "bodyText" .= content]


write :: Document -> IO()
write doc = 
  let
    fileName = "texFiles/" ++ (unpack $ docId doc) ++ ".tex"
    contents = unpack $ content doc
  in
    writeFile fileName contents
