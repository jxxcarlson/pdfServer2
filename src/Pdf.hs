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
        removeTexFileCmd = "rm inbox/*.tex"
        removePdfDetritus = "rm outbox/*.log outbox/*.aux  outbox/*.toc outbox/*.out"
    in
    do
        createPdf_ fileName  >>= \exitCode -> print exitCode
        -- system removePdfDetritus  >>= \exitCode -> print exitCode
        -- system removeTexFileCmd   >>= \exitCode -> print exitCode


createPdf_ :: String -> IO ()
createPdf_ fileName =
    let
        texFilename = "inbox/" ++ fileName
        cmd_ = "xelatex -output-directory=outbox -interaction=nonstopmode " ++ texFilename
        cmd = cmd_ ++ " ; " ++ cmd_ -- do it twice
    in
        system cmd >>= \exitCode -> print exitCode

-- HELPERS 

     
-- replace :: Eq a => [a] -> [a] -> [a] -> [a]
-- replace old new l = SU.join new . SU.split old $ l


