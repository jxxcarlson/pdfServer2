 {-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Pdf (create, createWithErrorPdf, PdfResult(..)) where

import Data.Text.Lazy (Text, unpack, pack)
import System.Process (system, readProcess)
import qualified Data.String.Utils as SU
import Text.RawString.QQ
import Data.List.Utils (replace)
import Data.List (isInfixOf)
import Document (Document, docId)
import GHC.Generics
import Data.Aeson
import System.Exit (ExitCode(..))
import System.IO (readFile)
import Control.Exception (catch)
import System.IO.Error (IOError)
import Control.Applicative ((<$>))
import System.Directory (doesFileExist, getFileSize)

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
        
        -- Check if PDF was actually created, regardless of exit code
        let outputPdfPath = "outbox/" ++ pdfFileName
        pdfExists <- doesFileExist outputPdfPath
        
        if pdfExists then do
            -- Check if the PDF is valid (not too small)
            fileSize <- getFileSize outputPdfPath
            
            -- If PDF is too small (less than 1KB), treat it as invalid
            if fileSize > 1000 then do
                -- PDF was created successfully and seems valid
                putStrLn $ "create: PDF created successfully at " ++ outputPdfPath ++ " (size: " ++ show fileSize ++ " bytes)"
                system removeInputs >>= \_ -> return ()
                system removeOuputJunk >>= \_ -> return ()
                system removeOldOutboxFiles >>= \_ -> return ()
                return $ PdfSuccess (pack pdfFileName)
            else do
                -- PDF is too small, likely corrupted
                putStrLn $ "create: PDF is too small (" ++ show fileSize ++ " bytes), treating as error"
                logContent <- readLogFile logFileName
                system removeInputs >>= \_ -> return ()
                system removeOldOutboxFiles >>= \_ -> return ()
                return $ PdfError (pack "PDF generation produced invalid output") (pack logContent)
        else do
            -- No PDF was created - this is a real error
            putStrLn $ "create: PDF creation failed - no output file"
            logContent <- readLogFile logFileName
            system removeInputs >>= \_ -> return ()
            -- Keep log files for failed compilations
            system removeOldOutboxFiles >>= \_ -> return ()
            return $ PdfError (pack $ "LaTeX compilation failed - no PDF generated") (pack logContent)

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
                        -- For the error log, we'll truncate to first 100 lines to avoid issues
                        logLines = take 100 $ lines (unpack logContent)
                        truncatedLog = unlines logLines
                        -- Look for specific errors in the log
                        hasImageError = any (\line -> "Unable to load picture" `isInfixOf` line || "image" `isInfixOf` line) logLines
                        hasBraceError = any (\line -> "Paragraph ended before" `isInfixOf` line || "Too many }" `isInfixOf` line) logLines
                        hasDivisionError = any (\line -> "Division by 0" `isInfixOf` line) logLines
                        -- Create specific error messages based on what we found
                        specificErrors = if hasImageError then
                                           "\\item \\textbf{Missing Image:} The document references an image that could not be loaded. Check that all image URLs are accessible.\n"
                                         else "" ++
                                         if hasBraceError then
                                           "\\item \\textbf{Unmatched Braces:} The document has mismatched \\{ \\} braces. Check your author field and other commands.\n"
                                         else "" ++
                                         if hasDivisionError then
                                           "\\item \\textbf{Division by Zero:} An image dimension calculation resulted in division by zero. This often happens with corrupted images.\n"
                                         else ""
                        errorTexContent = "\\documentclass{article}\n" ++
                                          "\\usepackage{geometry}\n" ++
                                          "\\geometry{letterpaper, margin=1in}\n" ++
                                          "\\usepackage{fancyvrb}\n" ++
                                          "\\begin{document}\n" ++
                                          "\\title{LaTeX Compilation Error}\n" ++
                                          "\\date{\\today}\n" ++
                                          "\\maketitle\n" ++
                                          "\\section{Error Summary}\n" ++
                                          "Failed to process document. The LaTeX compilation encountered errors.\n\n" ++
                                          "\\section{Specific Issues Found}\n" ++
                                          "\\begin{itemize}\n" ++
                                          specificErrors ++
                                          "\\end{itemize}\n\n" ++
                                          "\\section{Common Causes}\n" ++
                                          "\\begin{itemize}\n" ++
                                          "\\item Missing or misspelled LaTeX commands\n" ++
                                          "\\item Unmatched braces \\{ \\} or environments\n" ++
                                          "\\item Missing required packages\n" ++
                                          "\\item Invalid image references\n" ++
                                          "\\item Special characters that need escaping\n" ++
                                          "\\end{itemize}\n\n" ++
                                          "\\section{Error Log (First 100 lines)}\n" ++
                                          "\\begin{Verbatim}[fontsize=\\footnotesize,breaklines=true,breaksymbolleft={}]\n" ++
                                          truncatedLog ++ "\n" ++
                                          "\\end{Verbatim}\n" ++
                                          "\\end{document}"
                    -- Write the error TeX file
                    putStrLn $ "createWithErrorPdf: Writing error tex file: " ++ errorTexFileName
                    writeFile errorTexFileName errorTexContent
                    -- Compile it to PDF using system to avoid encoding issues
                    putStrLn "createWithErrorPdf: Compiling error PDF..."
                    exitCode <- system $ "xelatex -output-directory=outbox -interaction=nonstopmode " ++ errorTexFileName ++ " >/dev/null 2>&1"
                    putStrLn $ "createWithErrorPdf: XeLaTeX exit code for error PDF: " ++ show exitCode
                    -- Clean up the tex file regardless of success
                    system ("rm -f " ++ errorTexFileName ++ " 2>/dev/null") >>= \_ -> return ()
                    case exitCode of
                        ExitSuccess -> do
                            putStrLn "createWithErrorPdf: Error PDF created successfully"
                            -- Copy the error PDF to the expected filename
                            let errorPdfFileName = replace ".tex" ".pdf" ("error-" ++ fileName)
                            system $ "cp outbox/" ++ errorPdfFileName ++ " outbox/" ++ pdfFileName ++ " 2>/dev/null"
                            return (pack pdfFileName)
                        ExitFailure _ -> do
                            putStrLn "createWithErrorPdf: Error PDF compilation failed, creating fallback"
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
                            putStrLn "createWithErrorPdf: Compiling fallback PDF..."
                            exitCode2 <- system $ "xelatex -output-directory=outbox -interaction=nonstopmode " ++ errorTexFileName ++ " >/dev/null 2>&1"
                            system ("rm -f " ++ errorTexFileName ++ " 2>/dev/null") >>= \_ -> return ()
                            putStrLn $ "createWithErrorPdf: Fallback compilation exit code: " ++ show exitCode2
                            -- Even if fallback fails, we need to return something
                            -- Create a minimal PDF using pdflatex which is more robust
                            if exitCode2 /= ExitSuccess then do
                                let minimalTexFileName = "inbox/minimal-" ++ fileName
                                    minimalContent = "\\documentclass{article}\n\\begin{document}\nError: Failed to process document\n\\end{document}"
                                writeFile minimalTexFileName minimalContent
                                system $ "pdflatex -output-directory=outbox -interaction=nonstopmode " ++ minimalTexFileName ++ " >/dev/null 2>&1"
                                system ("rm -f " ++ minimalTexFileName ++ " 2>/dev/null") >>= \_ -> return ()
                                -- Copy the minimal PDF to the expected filename
                                let minimalPdfFileName = replace ".tex" ".pdf" ("minimal-" ++ fileName)
                                system $ "cp outbox/" ++ minimalPdfFileName ++ " outbox/" ++ pdfFileName ++ " 2>/dev/null"
                                return (pack pdfFileName)
                            else do
                                -- Copy the error PDF to the expected filename  
                                let errorPdfFileName = replace ".tex" ".pdf" ("error-" ++ fileName)
                                system $ "cp outbox/" ++ errorPdfFileName ++ " outbox/" ++ pdfFileName ++ " 2>/dev/null"
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


