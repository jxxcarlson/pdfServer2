{-# LANGUAGE OverloadedStrings #-}


-- https://dev.to/parambirs/how-to-write-a-haskell-web-servicefrom-scratch---part-3-5en6
-- https://adit.io/posts/2013-04-15-making-a-website-with-haskell.html

module Main where

import             Control.Monad.IO.Class (liftIO) -- liftIO :: IO a -> m a

import Web.Scotty
import Network.HTTP.Types
import Data.Text.Lazy (pack)
import Network.Wai.Middleware.Static ( (>->), addBase, noDots, staticPolicy )
import Web.Scotty
import Network.Wai.Middleware.Cors

import Pdf
import Document

main = scotty 3001 $ do
    middleware corsPolicy -- simpleCors

    post "/pdf" $ do
        
        document <- jsonData :: ActionM Document 
        liftIO $ Document.write document
        liftIO $ Pdf.create document
        text (Document.docId document)

    get "/pdflink/:id" $ do
        docId <- param "id"
        html $ pack $ Pdf.makeWebPage "http://localhost:3000" docId

    get "/pdf/:id" $ do
        docId <- param "id"
        file ("pdfFiles/" ++ docId ++ ".pdf")

    middleware $ staticPolicy (noDots >-> addBase "pdfFiles")


-- corsPolicy :: Middleware
corsPolicy = cors (const $ Just policy)
    where
      policy = simpleCorsResourcePolicy
        { corsOrigins  = Nothing
        , corsRequestHeaders = ["Content-Type"]  }