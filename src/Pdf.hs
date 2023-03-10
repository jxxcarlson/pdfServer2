{-# LANGUAGE QuasiQuotes #-}


module Pdf (create) where

import Data.Text.Lazy (Text, unpack)
import System.Process
import qualified Data.String.Utils as SU
import Text.RawString.QQ
import Data.List.Utils (replace)
import Document (Document, docId)


create :: Document -> IO()
create document =
    let
        fileName = unpack $ Document.docId document
        removeInputs = "rm inbox/*.tex image/*"
        removeOuputJunk = "rm outbox/*.log outbox/*.aux  outbox/*.log"
        removeOldOutboxFiles = "rm `find outbox -type f -mtime +1 -print`"

    in
    do
        createPdf_ fileName  >>= \exitCode -> print exitCode
        system removeInputs >>= \exitCode -> print exitCode
        system removeOuputJunk >>= \exitCode -> print exitCode
        system removeOldOutboxFiles >>= \exitCode -> print exitCode


createPdf_ :: String -> IO ()
createPdf_ fileName =
    let
        texFilename = "inbox/" ++ fileName
        cmd_ = "xelatex -output-directory=outbox -interaction=batchmode " ++ texFilename -- batchmode instead of nonstopmode
        cmd = cmd_ ++ " ; " ++ cmd_ -- do it twice
    in
        system cmd >>= \exitCode -> print exitCode

-- HELPERS 

     
-- replace :: Eq a => [a] -> [a] -> [a] -> [a]
-- replace old new l = SU.join new . SU.split old $ l


