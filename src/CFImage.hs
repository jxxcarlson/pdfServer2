{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module CFImage where


import Data.Aeson
import GHC.Generics
import GHC.IO.Exception
import System.Environment (getEnv)
import System.Process
import Control.Monad

data CFImage = CFImage
      {
        url             :: String
      , imageFilename   :: String
      , username        :: String
      } deriving (Show, Generic)


downloadImage :: CFImage -> IO()
downloadImage cfImage =
    do
     Control.Monad.void (downloadCFImage cfImage) -- write the image file to ./cf-image


downloadCFImage :: CFImage -> IO (GHC.IO.Exception.ExitCode)
downloadCFImage image =
   system ("wget -O " ++ "cf-image/" ++ imageFilename image ++ " " ++  url image)

-- wget -O cf-image/foo.jpg https://upload.wikimedia.org/wikipedia/commons/thumb/4/45/Eopsaltria_australis_-_Mogo_Campground.jpg/640px-Eopsaltria_australis_-_Mogo_Campground.jpg

-- DECODERS

-- Tell Aeson how to create a CFImage object from JSON string.
instance FromJSON CFImage

-- Tell Aeson how to convert a CFImage object to a JSON string.
instance ToJSON CFImage where


-- HELPERS


getFilenameFromImage :: CFImage -> String
getFilenameFromImage (CFImage _ imageFilename _) = imageFilename

getUrlFromImage :: CFImage -> String
getUrlFromImage (CFImage url _ _ ) = url

updateCFImage :: String -> CFImage -> CFImage
updateCFImage newUrl cfImage =
     cfImage { url = newUrl }


