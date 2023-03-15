{-# LANGUAGE OverloadedStrings #-}

module Image where

import Data.Text.Lazy ( Text )
import Data.Text
import Data.Text.Lazy.Encoding
import Data.Aeson
import Control.Applicative
import Data.List.Split
import Data.List.Utils (replace)
import GHC.IO.Exception
import System.Process

data CFImage = CFImage
      {
        url        :: String
      , filename   :: String
      , username   :: String
      } deriving Show



prepareCFImage :: CFImage -> IO()
prepareCFImage cfImage =
    do
      downloadCFImage cfImage >> return () -- write the image file to ./cf-image
  

downloadCFImage :: CFImage -> IO (GHC.IO.Exception.ExitCode)
downloadCFImage image =
   system ("wget -O " ++ "cf-image/" ++ (filename image) ++ " " ++  (url image))


-- Tell Aeson how to create a CFImage object from JSON string.
instance FromJSON CFImage where
     parseJSON (Object v) = CFImage <$>
                            v .:  (Data.Text.pack "url") <*> 
                            v .:  (Data.Text.pack "filename") <*>
                            v .:  (Data.Text.pack "username") 
                           
 

-- Tell Aeson how to convert a CFImage object to a JSON string.
instance ToJSON CFImage where
     toJSON (CFImage url filename username) =
         object ["url" .= url,
                 "filename" .= filename,
                 "username" .= username
                 ]

