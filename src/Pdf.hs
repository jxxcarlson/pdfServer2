{-# LANGUAGE QuasiQuotes #-}


module Pdf (create,   makeWebPage) where

import Data.Text.Lazy (Text, unpack)
import System.Process
import qualified Data.String.Utils as SU
import Text.RawString.QQ
import Data.List
import Document (Document, docId)


create :: Document -> IO()
create document =
    let
        fileName = unpack $ Document.docId document
        removeTexFileCmd = "rm texFiles/*.tex"
        removePdfDetritus = "rm pdfFiles/*.log pdfFiles/*.aux  pdfFiles/*.toc pdfFiles/*.out"
    in
    do
        createPdf_ fileName  >>= \exitCode -> print exitCode
        system removePdfDetritus  >>= \exitCode -> print exitCode
        system removeTexFileCmd   >>= \exitCode -> print exitCode


createPdf_ :: String -> IO ()
createPdf_ fileName =
    let
        texFile = "texFiles/" ++ fileName
        cmd_ = "xelatex -output-directory=pdfFiles -interaction=nonstopmode " ++ texFile
        cmd = cmd_ ++ " ; " ++ cmd_
    in
        system cmd >>= \exitCode -> print exitCode

-- HELPERS 

makeWebPage :: String -> String -> String
makeWebPage server fileName = 
    replace "SERVER" server $ replace "FILENAME" fileName template


rep a b s@(x:xs) = if Data.List.isPrefixOf a s

                     -- then, write 'b' and replace jumping 'a' substring
                     then b++rep a b (drop (length a) s)

                     -- then, write 'x' char and try to replace tail string
                     else x:rep a b xs

rep _ _ [] = []    





     
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace old new l = SU.join new . SU.split old $ l



template :: String
template = 
    [r|
        <html>
        <head>
        <title>PDF file</title>
        </head>
        <body style="background-color: #444">

        <div style="margin-top: 90px; margin-left: 90px; width: 175px; height: 80x; padding: 15px; padding-left: 40px; background-color: #eeeeff">
            <p>Here is your <a href="SERVER/pdf/FILENAME">PDF File</a></p>
        </div>

        </body>
        </html>
   |]