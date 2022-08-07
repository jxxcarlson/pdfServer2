{-# LANGUAGE QuasiQuotes #-}


module Pdf (create,  cleanup, remove, makeWebPage) where

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
        moveDownCmd = "mv texFiles/image texFiles/tmp/image"
    in
    do
        createPdf_ fileName
        -- createPdf_ fileName
        -- cleanup fileName
        -- system moveDownCmd >>= \exitCode -> print exitCode

createPdf_ :: String -> IO ()
createPdf_ fileName =
    let
        texFile = "texFiles/" ++ fileName
        cmd_ = "xelatex -output-directory=pdfFiles -interaction=nonstopmode " ++ texFile
        -- cmd_ = "xelatex -interaction=nonstopmode " ++ texFile
        cmd = cmd_ ++ " ; " ++ cmd_
    in
        system cmd_ >>= \exitCode -> print exitCode


remove :: Text -> IO ()
remove fileName =
    let
        f = unpack fileName
        cmd1 = "rm texFiles/" ++ f ++ ".tex"
        cmd2 = "rm pdfFiles/" ++ f ++ ".pdf"
        cmd3 = "rm texFiles/" ++ f ++ "_image_manifest.txt"
        cmd = cmd1  ++ "; " ++ cmd2++ "; " ++ cmd3
    in
        system cmd >>= \exitCode -> print exitCode

cleanup :: String -> IO ()
cleanup fileName =
    let
        cmd_ f ext = "rm pdfFiles/" ++ f ++ ext
        cmd = cmd_ fileName ".log" ++ ";" 
               ++ cmd_ fileName ".aux" ++ ";" 
               ++ cmd_ fileName ".out" ++ ";" 
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