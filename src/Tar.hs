{-# LANGUAGE QuasiQuotes #-}


module Tar (create) where

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
        tarFile = replace ".tex" ".tar" fileName
        removeTexFileCmd = "rm inbox/*.tex"
        removePdfDetritus = "rm outbox/*.log outbox/*.aux  outbox/*.toc outbox/*.out"
        createTarFile = "tar -cf outbox/" ++ tarFile ++ " -C inbox/tmp ."
    in
    (system createTarFile) >>= \exitCode -> print exitCode
        -- system removePdfDetritus  >>= \exitCode -> print exitCode
        -- system removeTexFileCmd   >>= \exitCode -> print exitCode


-- -- HELPERS 

     
-- replace :: Eq a => [a] -> [a] -> [a] -> [a]
-- replace old new l = SU.join new . SU.split old $ l


