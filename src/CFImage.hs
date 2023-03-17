{-# LANGUAGE OverloadedStrings #-}

module CFImage where


import System.Environment (getEnv)
import Data.Text.Lazy ( Text, pack )
import Data.Text
import Data.Text.Lazy.Encoding
import Data.Aeson
import Control.Applicative
import Data.List.Split
import Data.List.Utils (replace)
import Control.Applicative ((<*>), (<$>), empty)
import Control.Monad (mzero, void)
import GHC.IO.Exception
import System.Process
import System.FilePath
import System.Path
import Network.HTTP.Client
import Network.HTTP.Client.TLS
-- import Network.HTTP.Client.MultipartFormData


import qualified Data.ByteString.Lazy.Char8 as LBS
-- import qualified Network.HTTP.Client        as Client
-- import qualified Network.HTTP.Client.TLS    as TLS
import qualified Network.HTTP.Client.MultipartFormData  as MultiPart


import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy as TL



data CFImage = CFImage
      {
        url             :: String
      , imageFilename   :: String
      , username        :: String
      } deriving Show

getFilenameFromImage :: CFImage -> String
getFilenameFromImage (CFImage _ imageFilename _) = imageFilename

getUrlFromImage :: CFImage -> String
getUrlFromImage (CFImage url _ _ ) = url

updateCFImage :: String -> CFImage -> CFImage
updateCFImage newUrl cfImage = 
     cfImage { url = newUrl }




downloadImage :: CFImage -> IO()
downloadImage cfImage =
    do
      downloadCFImage cfImage >> return () -- write the image file to ./cf-image
  

downloadCFImage :: CFImage -> IO (GHC.IO.Exception.ExitCode)
downloadCFImage image =
   system ("wget -O " ++ "cf-image/" ++ (imageFilename image) ++ " " ++  (url image))


-- Tell Aeson how to create a CFImage object from JSON string.
instance FromJSON CFImage where
     parseJSON (Object v) = CFImage <$>
                            v .:  (Data.Text.pack "url") <*> 
                            v .:  (Data.Text.pack "imageFilename") <*>
                            v .:  (Data.Text.pack "username") 
                           
 

-- Tell Aeson how to convert a CFImage object to a JSON string.
instance ToJSON CFImage where
     toJSON (CFImage url filename username) =
         object ["url" .= url,
                 "imageFilename" .= filename,
                 "username" .= username
                 ]
