{-# LANGUAGE OverloadedStrings #-}


-- https://dev.to/parambirs/how-to-write-a-haskell-web-servicefrom-scratch---part-3-5en6
-- https://adit.io/posts/2013-04-15-making-a-website-with-haskell.html

module Main where

import Control.Monad.IO.Class (liftIO) -- liftIO :: IO a -> m a

import Web.Scotty
import Network.HTTP.Types
import Network.Wai.Middleware.Static ( (>->), addBase, noDots, staticPolicy )
import Web.Scotty
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger
import System.Process
--  import Data.List.Utils (replace)
import Data.Text.Lazy (pack, unpack, replace, Text)
import Pdf
import Tar
import Document (Document, writeTeXSourceFile, prepareData, docId)

main = scotty 3000 $ do
    middleware corsPolicy 
    middleware logStdoutDev

    post "/pdf" $ do
        
        document <- jsonData :: ActionM Document 
        liftIO $ Document.prepareData document
        liftIO $ Pdf.create document
        text  (textReplace ".tex" ".pdf" (Document.docId document))

    post "/tar" $ do
        document <- jsonData :: ActionM Document 
        liftIO $ Document.prepareData document
        liftIO $ Tar.create document
        text (Document.docId document)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   

    get "/pdf/:id" $ do
        docId <- param "id"
        file ("outbox/" ++ docId)


    get "/tar/:id" $ do
        docId <- param "id"
        file ("outbox/" ++ (unpack docId) )

    get "/hello" $ do
        html $ mconcat ["Yes, I am still here\n"]


    post "/hello" $ do
       text "Yes, I am still here\n"

    middleware $ staticPolicy (noDots >-> addBase "outbox")


textReplace :: String -> String -> Text -> Text
textReplace src target text = 
    replace (pack src) (pack target) text

-- corsPolicy :: Middleware
corsPolicy = cors (const $ Just policy)
    where
      policy = simpleCorsResourcePolicy
        { corsOrigins  = Nothing
        , corsRequestHeaders = ["Content-Type"]  }

