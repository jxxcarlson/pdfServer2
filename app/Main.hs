{-# LANGUAGE OverloadedStrings #-}


-- https://dev.to/parambirs/how-to-write-a-haskell-web-servicefrom-scratch---part-3-5en6
-- https://adit.io/posts/2013-04-15-making-a-website-with-haskell.html

module Main where

import             Control.Monad.IO.Class (liftIO) -- liftIO :: IO a -> m a

import Web.Scotty
import Network.HTTP.Types
import Data.Text.Lazy (pack, Text)
import Network.Wai.Middleware.Static ( (>->), addBase, noDots, staticPolicy )
import Web.Scotty
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger
import System.Process


import Pdf
import Document (Document, write, writeImageManifest, cleanImages, docId)

main = scotty 80 $ do
    middleware corsPolicy 
    middleware logStdoutDev

    post "/pdf" $ do
        
        document <- jsonData :: ActionM Document 
        liftIO $ Document.write document
        liftIO $ Document.writeImageManifest document
        liftIO $ Pdf.create document
        text (Document.docId document)

    post "/clean/:id" $ do
       docId <- param "id" 
       liftIO $ Document.cleanImages docId   
       liftIO $ Pdf.remove docId                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 

    get "/pdflink/:id" $ do
        docId <- param "id"
        html $ pack $ Pdf.makeWebPage "http://localhost:3000" docId

    get "/pdf/:id" $ do
        docId <- param "id"
        file ("pdfFiles/" ++ docId ++ ".pdf")

    get "/hello" $ do
        html $ mconcat ["<p>Yes, I am still here.</p"]

    middleware $ staticPolicy (noDots >-> addBase "pdfFiles")




-- corsPolicy :: Middleware
corsPolicy = cors (const $ Just policy)
    where
      policy = simpleCorsResourcePolicy
        { corsOrigins  = Nothing
        , corsRequestHeaders = ["Content-Type"]  }

