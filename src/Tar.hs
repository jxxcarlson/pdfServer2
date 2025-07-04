{-# LANGUAGE QuasiQuotes #-}


module Tar (create) where

import Data.Text.Lazy (Text, unpack)
import System.Process
import qualified Data.String.Utils as SU
import Text.RawString.QQ
import Data.List.Utils (replace)
import Data.List (isSuffixOf)
import Document (Document, docId)


create :: Document -> IO()
create document =
    let
        fileName = unpack $ Document.docId document
        tarFile = if ".tar" `isSuffixOf` fileName 
                  then fileName 
                  else replace ".tex" ".tar" fileName
        baseNameNoExt = replace ".tar" "" $ replace ".tex" "" fileName
        removeTexFileCmd = "rm inbox/*.tex"
        removePdfDetritus = "rm outbox/*.log outbox/*.aux  outbox/*.toc outbox/*.out"
        -- First create a temporary directory structure
        setupCmd = "mkdir -p inbox/tmp/" ++ baseNameNoExt ++ " && " ++
                   "cp inbox/" ++ fileName ++ " inbox/tmp/" ++ baseNameNoExt ++ "/ && " ++
                   "cp -r image/* inbox/tmp/" ++ baseNameNoExt ++ "/ 2>/dev/null || true"
        -- Create TAR from the temporary directory
        createTarFile = "tar -cf outbox/" ++ tarFile ++ " -C inbox/tmp " ++ baseNameNoExt
        -- Clean up temporary directory
        cleanupCmd = "rm -rf inbox/tmp"
    in
    do
        system setupCmd >>= \exitCode -> print exitCode
        system createTarFile >>= \exitCode -> print exitCode
        system cleanupCmd >>= \exitCode -> print exitCode
        -- system removePdfDetritus  >>= \exitCode -> print exitCode
        -- system removeTexFileCmd   >>= \exitCode -> print exitCode


-- -- HELPERS 

     
-- replace :: Eq a => [a] -> [a] -> [a] -> [a]
-- replace old new l = SU.join new . SU.split old $ l


