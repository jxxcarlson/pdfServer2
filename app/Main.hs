{-# LANGUAGE OverloadedStrings #-}

module Main where

import             Control.Monad.IO.Class (liftIO) -- liftIO :: IO a -> m a
import Lib

import Web.Scotty
import Network.HTTP.Types

import Process
import Document

main = scotty 3000 $ do
   -- post article (json)
  post "/pdf" $ do
    document <- jsonData :: ActionM Document -- Decode body of the POST request as an Article object
    liftIO $ Document.write document
    liftIO $ Process.createPdf document