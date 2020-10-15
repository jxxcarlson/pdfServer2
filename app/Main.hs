{-# LANGUAGE OverloadedStrings #-}


-- https://dev.to/parambirs/how-to-write-a-haskell-web-servicefrom-scratch---part-3-5en6
-- https://adit.io/posts/2013-04-15-making-a-website-with-haskell.html

module Main where

import             Control.Monad.IO.Class (liftIO) -- liftIO :: IO a -> m a

import Web.Scotty
import Network.HTTP.Types
import Network.Wai.Middleware.Static

import Pdf
import Document

main = scotty 3000 $ do
    post "/pdf" $ do
        document <- jsonData :: ActionM Document 
        liftIO $ Document.write document
        liftIO $ Pdf.create document
        text (Document.docId document)

    get "/pdf/:id" $ do
        docId <- param "id"
        file ("pdfFiles/" ++ docId ++ ".pdf")

    middleware $ staticPolicy (noDots >-> addBase "pdfFiles")


