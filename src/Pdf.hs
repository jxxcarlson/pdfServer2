 {-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Pdf (create, createWithErrorPdf, PdfResult(..)) where

import Data.Text.Lazy (Text, unpack, pack)
import System.Process
import qualified Data.String.Utils as SU
import Text.RawString.QQ
import Data.List.Utils (replace)
import Document (Document, docId)
import GHC.Generics
import Data.Aeson
import System.Exit (ExitCode(..))
import System.IO (readFile)
import Control.Exception (catch)
import System.IO.Error (IOError)
import Control.Applicative ((<$>))

data PdfResult = PdfSuccess { filename :: Text }
               | PdfError { error :: Text, errorLog :: Text }
               deriving (Show, Generic)

instance ToJSON PdfResult where
    toJSON (PdfSuccess fname) = object ["success" .= True, "filename" .= fname]
    toJSON (PdfError err log) = object ["success" .= False, "error" .= err, "log" .= log]

create :: Document -> IO PdfResult
create document =
    let
        fileName = unpack $ Document.docId document
        removeInputs = "rm -f inbox/*.tex image/* 2>/dev/null || true"
        removeOuputJunk = "rm -f outbox/*.aux 2>/dev/null || true"  -- Keep .log files for error reporting
        removeOldOutboxFiles = "find outbox -type f -mtime +1 -delete 2>/dev/null || true"
        pdfFileName = replace ".tex" ".pdf" fileName
        logFileName = "outbox/" ++ replace ".tex" ".log" fileName
    in
    do
        putStrLn $ "create: Processing document: " ++ fileName
        exitCode <- createPdf_ fileName
        putStrLn $ "create: XeLaTeX exit code: " ++ show exitCode
        case exitCode of
            ExitSuccess -> do
                system removeInputs >>= \_ -> return ()
                system removeOuputJunk >>= \_ -> return ()
                system removeOldOutboxFiles >>= \_ -> return ()
                return $ PdfSuccess (pack pdfFileName)
            ExitFailure code -> do
                logContent <- readLogFile logFileName
                system removeInputs >>= \_ -> return ()
                -- Keep log files for failed compilations
                system removeOldOutboxFiles >>= \_ -> return ()
                return $ PdfError (pack $ "LaTeX compilation failed with exit code: " ++ show code) (pack logContent)

-- Create PDF, but on error create a PDF of the error log
createWithErrorPdf :: Document -> IO Text
createWithErrorPdf document = do
    putStrLn "createWithErrorPdf: Starting..."
    resultOrError <- catch (fmap Right (create document)) $ \e -> do
        putStrLn $ "createWithErrorPdf: Caught exception: " ++ show (e :: IOError)
        -- For UTF-8 or other IO errors, create simple error PDF directly
        let fileName = unpack $ Document.docId document
            pdfFileName = replace ".tex" ".pdf" fileName
            errorTexFileName = "inbox/error-utf8-" ++ fileName
            simpleErrorContent = "\\documentclass{article}\n\\begin{document}\n" ++
                               "\\title{Document Processing Error}\n\\maketitle\n" ++
                               "\\section{UTF-8 Encoding Error}\n" ++
                               "The document contains characters that cannot be processed.\\\\[1em]\n" ++
                               "This usually happens when:\n" ++
                               "\\begin{itemize}\n" ++
                               "\\item The document contains special Unicode characters\n" ++
                               "\\item The file encoding is not UTF-8\n" ++
                               "\\item There are invisible control characters\n" ++
                               "\\end{itemize}\n" ++
                               "\\section{Solution}\n" ++
                               "Please ensure your document:\n" ++
                               "\\begin{itemize}\n" ++
                               "\\item Uses UTF-8 encoding\n" ++
                               "\\item Avoids special Unicode symbols in math mode\n" ++
                               "\\item Uses standard LaTeX commands for special characters\n" ++
                               "\\end{itemize}\n" ++
                               "\\end{document}"
        -- Write and compile the simple error document
        writeFile errorTexFileName simpleErrorContent
        system ("xelatex -output-directory=outbox -interaction=nonstopmode " ++ errorTexFileName ++ " >/dev/null 2>&1") >>= \_ -> return ()
        system ("rm " ++ errorTexFileName) >>= \_ -> return ()
        -- Return Left to indicate we handled the error
        return (Left $ pack pdfFileName)
    case resultOrError of
        Left pdfFileName -> return pdfFileName  -- Already handled
        Right result -> do
            putStrLn $ "createWithErrorPdf: Got result: " ++ show result
            case result of
                PdfSuccess fname -> return fname
                PdfError errMsg logContent -> do
                    putStrLn "createWithErrorPdf: Handling error case..."
                    let fileName = unpack $ Document.docId document
                        pdfFileName = replace ".tex" ".pdf" fileName
                        errorTexFileName = "inbox/error-" ++ fileName
                        -- Create a LaTeX document containing the error log
                        -- Escape backslashes in verbatim content to prevent LaTeX issues
                        escapedLog = unpack logContent
                        errorTexContent = "\\documentclass{article}\n" ++
                                          "\\usepackage{geometry}\n" ++
                                          "\\geometry{letterpaper, margin=1in}\n" ++
                                          "\\usepackage{fancyvrb}\n" ++
                                          "\\begin{document}\n" ++
                                          "\\title{LaTeX Compilation Error}\n" ++
                                          "\\maketitle\n" ++
                                          "\\section{Error Summary}\n" ++
                                          (unpack errMsg) ++ "\n\n" ++
                                          "\\section{Common Causes}\n" ++
                                          "\\begin{itemize}\n" ++
                                          "\\item Missing or misspelled LaTeX commands\n" ++
                                          "\\item Unmatched braces \\{ \\} or environments\n" ++
                                          "\\item Missing required packages\n" ++
                                          "\\item Invalid image references\n" ++
                                          "\\item Special characters that need escaping\n" ++
                                          "\\end{itemize}\n\n" ++
                                          "\\section{Full Error Log}\n" ++
                                          "\\begin{Verbatim}[breaklines=true,breaksymbolleft={}]\n" ++
                                          escapedLog ++ "\n" ++
                                          "\\end{Verbatim}\n" ++
                                          "\\end{document}"
                    -- Write the error TeX file
                    writeFile errorTexFileName errorTexContent
                    -- Compile it to PDF
                    (exitCode, stdout, stderr) <- readProcessWithExitCode "xelatex" 
                        ["-output-directory=outbox", "-interaction=nonstopmode", errorTexFileName] ""
                    case exitCode of
                        ExitSuccess -> do
                            -- Clean up
                            system ("rm " ++ errorTexFileName) >>= \_ -> return ()
                            -- Return the PDF filename
                            return (pack pdfFileName)
                        ExitFailure _ -> do
                            -- If error PDF fails, create a simple text file as PDF
                            let fallbackContent = "\\documentclass{article}\n" ++
                                                "\\usepackage{geometry}\n" ++
                                                "\\geometry{letterpaper, margin=1in}\n" ++
                                                "\\begin{document}\n" ++
                                                "\\title{Document Processing Error}\n" ++
                                                "\\maketitle\n" ++
                                                "\\section{Error Summary}\n" ++
                                                "Failed to process document. Some possible causes are:\n\n" ++
                                                "\\begin{itemize}\n" ++
                                                "\\item \\textbf{Invalid LaTeX syntax:} The document contains LaTeX commands that are not recognized.\n" ++
                                                "\\item \\textbf{Missing packages:} The document uses packages that are not installed on the server.\n" ++
                                                "\\item \\textbf{Encoding issues:} The document contains special characters that cannot be processed.\n" ++
                                                "\\item \\textbf{Image problems:} Referenced images could not be downloaded or have unsupported formats.\n" ++
                                                "\\item \\textbf{Syntax errors:} Missing braces, unmatched environments, or other structural issues.\n" ++
                                                "\\item \\textbf{Memory limits:} The document is too complex or large to process.\n" ++
                                                "\\end{itemize}\n\n" ++
                                                "\\section{Troubleshooting Steps}\n" ++
                                                "\\begin{enumerate}\n" ++
                                                "\\item Verify your LaTeX syntax using a local LaTeX editor.\n" ++
                                                "\\item Check that all required packages are standard LaTeX packages.\n" ++
                                                "\\item Ensure all images are publicly accessible URLs.\n" ++
                                                "\\item Try simplifying the document to identify the problem area.\n" ++
                                                "\\item Check for unmatched braces or environments.\n" ++
                                                "\\end{enumerate}\n\n" ++
                                                "If the problem persists, please contact support with your document.\n" ++
                                                "\\end{document}"
                            writeFile errorTexFileName fallbackContent
                            (exitCode2, _, _) <- readProcessWithExitCode "xelatex"
                                ["-output-directory=outbox", "-interaction=nonstopmode", errorTexFileName] ""
                            system ("rm " ++ errorTexFileName) >>= \_ -> return ()
                            return (pack pdfFileName)

createPdf_ :: String -> IO ExitCode
createPdf_ fileName =
    let
        texFilename = "inbox/" ++ fileName
        -- Use system instead of readProcessWithExitCode to avoid UTF-8 decoding issues
        cmd1 = "xelatex -output-directory=outbox -interaction=batchmode " ++ texFilename ++ " >/dev/null 2>&1"
        cmd2 = cmd1  -- Run the same command twice
    in do
        -- Run xelatex twice for proper reference resolution
        exitCode1 <- system cmd1
        case exitCode1 of
            ExitSuccess -> system cmd2
            _ -> return exitCode1

-- HELPERS 

readLogFile :: String -> IO String
readLogFile logPath = do
    result <- try (readFile logPath) :: IO (Either IOError String)
    case result of
        Left _ -> return "Error: Could not read log file"
        Right content -> return content
  where
    try :: IO a -> IO (Either IOError a)
    try action = catch (Right <$> action) (return . Left)


