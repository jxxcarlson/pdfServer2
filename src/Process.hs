{-# LANGUAGE QuasiQuotes #-}


module Process (createPdf,  cleanup, remove) where

import Data.Text.Lazy (unpack)
import System.Process
import qualified Data.String.Utils as SU
import Text.RawString.QQ
import Data.List
import Document


createPdf :: Document -> IO()
createPdf document =
    let
        fileName = unpack $ Document.docId document
    in
    do
        createPdf_ fileName
        createPdf_ fileName
        cleanup fileName
        writeWebPage fileName


remove :: String -> IO ()
remove fileName =
    let
        cmd1 f = "rm texFiles/" ++ f ++ ".tex"
        cmd2 f = "rm pdfFiles/" ++ f ++ ".pdf"
        cmd3 f = "rm pdfFiles/" ++ f ++ ".html"
        cmd = cmd1 fileName  ++ ";" 
               ++ cmd2 fileName ++ ";" 
               ++ cmd3 fileName
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

makeWebPage :: String -> String
makeWebPage fileName = 
    replace "FILENAME" fileName template

writeWebPage :: String -> IO()
writeWebPage fileName = 
    let
        webPageFilePath = "pdfFiles/" ++ fileName ++ ".html"
    in
        writeFile webPageFilePath $ makeWebPage fileName

rep a b s@(x:xs) = if Data.List.isPrefixOf a s

                     -- then, write 'b' and replace jumping 'a' substring
                     then b++rep a b (drop (length a) s)

                     -- then, write 'x' char and try to replace tail string
                     else x:rep a b xs

rep _ _ [] = []    



createPdf_ :: String -> IO ()
createPdf_ fileName =
    let
        texFile = "texFiles/" ++ fileName ++ ".tex"
        cmd_ = "pdflatex -output-directory=pdfFiles " ++ texFile
        cmd = cmd_ ++ " ; " ++ cmd_
    in
        system cmd >>= \exitCode -> print exitCode


     
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
            <p>Here is your <a href="FILENAME.pdf">PDF File</a></p>
        </div>

        </body>
        </html>
   |]
