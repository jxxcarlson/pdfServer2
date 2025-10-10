 {-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pdf (create, createWithErrorPdf, createWithFailedImages, createWithFilteredErrors, PdfResult(..)) where

import Data.Text.Lazy (Text, unpack, pack)
import System.Process (system, readProcess)
import qualified Data.String.Utils as SU
import Text.RawString.QQ
import Data.List.Utils (replace)
import Data.List (isInfixOf, isPrefixOf, nubBy)
import Data.Maybe (mapMaybe, listToMaybe)
import Document (Document, ImageElement(..), docId, urlList)
import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import System.Exit (ExitCode(..))
import System.IO (readFile)
import Control.Exception (catch)
import System.IO.Error (IOError)
import Control.Applicative ((<$>))
import System.Directory (doesFileExist, getFileSize)
import qualified Data.Map.Strict as Map
import Data.Char (isDigit, isSpace)
import Text.Read (readMaybe)
import qualified Concordance
import System.IO.Unsafe (unsafePerformIO)

data PdfResult = PdfSuccess { filename :: Text }
               | PdfError { error :: Text, errorLog :: Text }
               | PdfWithErrors { pdfFile :: Text, errorPdfFile :: Text, errorMsg :: Text }
               deriving (Show, Generic)

instance ToJSON PdfResult where
    toJSON (PdfSuccess fname) = object ["success" .= True, "filename" .= fname]
    toJSON (PdfError err log) = object ["success" .= False, "error" .= err, "log" .= log]
    toJSON (PdfWithErrors pdf errorPdf msg) = object
        ["success" .= False, "pdfFile" .= pdf, "errorPdfFile" .= errorPdf, "error" .= msg]

data ErrorRecord = ErrorRecord
    { scriptaLine :: Int
    , latexLine :: Int
    , latexBegin :: Int
    , latexEnd :: Int
    , latexText :: String
    } deriving (Show, Generic)

instance ToJSON ErrorRecord where
    toJSON (ErrorRecord sl ll lb le lt) = object
        ["scripta-line" .= sl, "latex-line" .= ll, "latex-begin" .= lb, "latex-end" .= le, "latex-text" .= lt]

create :: Document -> IO PdfResult
create document =
    let
        fileName = unpack $ Document.docId document
        removeInputs = "rm -f inbox/*.tex image/* 2>/dev/null || true"
        removeOuputJunk = "rm -f outbox/*.aux 2>/dev/null || true"  -- Keep .log files for error reporting
        removeOldOutboxFiles = "find outbox -type f -mtime +1 -delete 2>/dev/null || true"
        pdfFileName = replace ".tex" ".pdf" fileName
        logFileName = "outbox/" ++ replace ".tex" ".log" fileName
        allImages = urlList document  -- Get all images for URL mapping
    in
    do
        putStrLn $ "create: Processing document: " ++ fileName
        -- JC:temporary - Save a copy of the LaTeX file for debugging
        system "mkdir -p save 2>/dev/null || true" >>= \_ -> return ()
        system ("cp inbox/" ++ fileName ++ " save/" ++ fileName ++ " 2>/dev/null || true") >>= \_ -> return ()

        exitCode <- createPdf_ fileName
        putStrLn $ "create: XeLaTeX exit code: " ++ show exitCode

        -- Check for timeout
        case exitCode of
            ExitFailure 124 -> do
                putStrLn $ "create: XeLaTeX timed out after 30 seconds"
                system removeInputs >>= \_ -> return ()
                system removeOldOutboxFiles >>= \_ -> return ()
                return $ PdfError (pack "LaTeX compilation timed out") (pack "The document took too long to compile (>30 seconds). This usually happens with:\n- Infinite loops in LaTeX macros\n- Extremely large documents\n- Missing fonts causing font substitution loops\n- Malformed TikZ or other graphics code\n- Recursive includes")
            _ -> do
                -- Check if PDF was actually created
                let outputPdfPath = "outbox/" ++ pdfFileName
                pdfExists <- doesFileExist outputPdfPath

                -- If XeLaTeX returned non-zero exit code, treat as error even if PDF exists
                -- (damaged PDFs can still be created with non-zero exit codes)
                case exitCode of
                    ExitSuccess -> do
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
                    ExitFailure _ -> do
                        -- Non-zero exit code means errors occurred
                        -- Check if a PDF was created (even if damaged)
                        putStrLn $ "create: XeLaTeX reported errors (exit code: " ++ show exitCode ++ ")"

                        logContent <- readLogFile logFileName

                        if pdfExists then do
                            fileSize <- getFileSize outputPdfPath
                            putStrLn $ "create: PDF file exists but may be damaged (size: " ++ show fileSize ++ " bytes)"

                            -- Generate an error report text file alongside the damaged PDF
                            let errorTextFileName = replace ".pdf" "-errors.txt" pdfFileName
                            generateErrorReportText fileName errorTextFileName logContent allImages

                            -- JC TODO: temporarily disable cleanup to keep artifacts for debugging
                            system removeInputs >>= \_ -> return ()
                            system removeOldOutboxFiles >>= \_ -> return ()

                            -- Return both the (possibly damaged) PDF and the error report text file
                            return $ PdfWithErrors (pack pdfFileName) (pack errorTextFileName)
                                (pack "LaTeX compilation had errors - PDF may be incomplete")
                        else do
                            putStrLn $ "create: No PDF file was created"
                            system removeInputs >>= \_ -> return ()
                            system removeOldOutboxFiles >>= \_ -> return ()
                            return $ PdfError (pack "LaTeX compilation failed - no PDF generated") (pack logContent)

-- Create PDF with failed images info
createWithFailedImages :: Document -> [ImageElement] -> IO Text
createWithFailedImages document failedImages = do
    putStrLn "createWithFailedImages: Starting..."
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
                        -- Check if it was a timeout
                        isTimeout = "timed out" `isInfixOf` (unpack errMsg)
                        -- Use the entire error log
                        logLines = lines (unpack logContent)
                        fullLog = unlines logLines
                        -- Look for specific errors in the log (if not timeout)
                        hasImageError = not isTimeout && any (\line -> "Unable to load picture" `isInfixOf` line || "image" `isInfixOf` line) logLines
                        hasBraceError = not isTimeout && any (\line -> "Paragraph ended before" `isInfixOf` line || "Too many }" `isInfixOf` line) logLines
                        hasDivisionError = not isTimeout && any (\line -> "Division by 0" `isInfixOf` line) logLines
                        -- Create specific error messages based on what we found
                        specificErrors = if isTimeout then
                                           "\\item \\textbf{Compilation Timeout:} The document took more than 30 seconds to compile and was terminated.\n" ++
                                           "\\item This usually indicates an infinite loop in LaTeX macros or extremely complex graphics.\n" ++
                                           "\\item Try simplifying the document or checking for recursive macro definitions.\n"
                                         else if hasImageError then
                                           "\\item \\textbf{Missing Image:} The document references an image that could not be loaded. Check that all image URLs are accessible.\n"
                                         else "" ++
                                         if hasBraceError then
                                           "\\item \\textbf{Unmatched Braces:} The document has mismatched \\{ \\} braces. Check your author field and other commands.\n"
                                         else "" ++
                                         if hasDivisionError then
                                           "\\item \\textbf{Division by Zero:} An image dimension calculation resulted in division by zero. This often happens with corrupted images.\n"
                                         else ""
                        -- Add failed images section if there are any
                        failedImagesSection = if not (null failedImages) then
                                                "\\section{Failed Image Downloads}\n" ++
                                                "The following images could not be downloaded and have been replaced with placeholders:\n\n" ++
                                                "\\begin{itemize}\n" ++
                                                concatMap (\img -> "\\item \\textbf{" ++ Document.filename img ++ "}\\\\\n" ++
                                                                  "\\quad URL: \\texttt{" ++ escapeLatex (Document.url img) ++ "}\n") failedImages ++
                                                "\\end{itemize}\n\n"
                                              else ""
                        errorTexContent = "\\documentclass{article}\n" ++
                                          "\\usepackage{geometry}\n" ++
                                          "\\geometry{letterpaper, margin=1in}\n" ++
                                          "\\begin{document}\n" ++
                                          "\\title{LaTeX Compilation Error}\n" ++
                                          "\\date{\\today}\n" ++
                                          "\\maketitle\n" ++
                                          "\\section{Error Summary}\n" ++
                                          (if isTimeout then "The document compilation was terminated after exceeding the 30-second time limit.\n\n"
                                           else "Failed to process document. The LaTeX compilation encountered errors.\n\n") ++
                                          "\\section{Specific Issues Found}\n" ++
                                          "\\begin{itemize}\n" ++
                                          specificErrors ++
                                          "\\end{itemize}\n\n" ++
                                          failedImagesSection ++
                                          (if isTimeout then
                                             "\\section{Common Timeout Causes}\n" ++
                                             "\\begin{itemize}\n" ++
                                             "\\item Infinite loops in LaTeX macro definitions\n" ++
                                             "\\item Extremely complex TikZ or PGF graphics\n" ++
                                             "\\item Missing fonts causing substitution loops\n" ++
                                             "\\item Very large documents with excessive content\n" ++
                                             "\\item Recursive \\\\input or \\\\include commands\n" ++
                                             "\\item Malformed table or array environments\n" ++
                                             "\\end{itemize}\n\n"
                                           else
                                             "\\section{Common Causes}\n" ++
                                             "\\begin{itemize}\n" ++
                                             "\\item Missing or misspelled LaTeX commands\n" ++
                                             "\\item Unmatched braces \\{ \\} or environments\n" ++
                                             "\\item Missing required packages\n" ++
                                             "\\item Invalid image references\n" ++
                                             "\\item Special characters that need escaping\n" ++
                                             "\\end{itemize}\n\n") ++
                                          (if isTimeout then
                                             "\\section{Timeout Information}\n" ++
                                             "No error log is available because the compilation was forcibly terminated.\\\\[1em]\n" ++
                                             "To debug, try:\n" ++
                                             "\\begin{enumerate}\n" ++
                                             "\\item Simplifying complex graphics or equations\n" ++
                                             "\\item Checking for recursive macro definitions\n" ++
                                             "\\item Testing with smaller portions of the document\n" ++
                                             "\\end{enumerate}\n"
                                           else
                                             "\\section{Complete Error Log}\n" ++
                                             "{\\small\n" ++
                                             "\\begin{verbatim}\n" ++
                                             fullLog ++ "\n" ++
                                             "\\end{verbatim}\n" ++
                                             "}\n") ++
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
        -- Use timeout to prevent hanging (30 seconds per run)
        -- Exit code 124 means timeout occurred
        cmd = "timeout 30 xelatex -output-directory=outbox -interaction=batchmode " ++ texFilename ++ " >/dev/null 2>&1"
    in do
        -- Run xelatex first time
        exitCode1 <- system cmd
        case exitCode1 of
            ExitFailure 124 -> return (ExitFailure 124)  -- Timeout on first run, don't continue
            _ -> do
                -- First run completed (success or error), run second time
                exitCode2 <- system cmd
                case exitCode2 of
                    ExitFailure 124 -> return (ExitFailure 124)  -- Timeout on second run
                    _ -> do
                        -- Second run completed, run third time for complete TOC/references
                        -- This is especially important for books with complex cross-references
                        exitCode3 <- system cmd
                        case exitCode3 of
                            ExitFailure 124 -> return (ExitFailure 124)  -- Timeout on third run
                            _ -> return exitCode3  -- Return third run's exit code

-- HELPERS

-- Generate a standalone error report as a text file
generateErrorReportText :: String -> String -> String -> [ImageElement] -> IO ()
generateErrorReportText originalFileName errorTextFileName logContent failedImages = do
    -- Read the LaTeX source for concordance
    let latexSourcePath = "inbox/" ++ originalFileName
    putStrLn $ "generateErrorReportText: Reading LaTeX source from: " ++ latexSourcePath
    latexSource <- catch (readFile latexSourcePath) $ \(e :: IOError) -> do
        putStrLn $ "generateErrorReportText: Failed to read LaTeX source: " ++ show e
        return ""
    putStrLn $ "generateErrorReportText: Read " ++ show (length latexSource) ++ " bytes of LaTeX source"
    let concordanceMap = buildConcordanceMap logContent latexSource
    putStrLn $ "generateErrorReportText: Built concordance with " ++ show (Map.size concordanceMap) ++ " entries"
    putStrLn $ "generateErrorReportText: Concordance keys: " ++ show (Map.keys concordanceMap)

    -- Extract some error line numbers from the log for debugging
    let logLines = lines logContent
        errorLineNumbers = mapMaybe extractLineNumber logLines
    putStrLn $ "generateErrorReportText: Found " ++ show (length errorLineNumbers) ++ " error line references in log"
    putStrLn $ "generateErrorReportText: Sample error lines: " ++ show (take 10 errorLineNumbers)

    let filteredLog = filterLatexLogWithUrls logContent latexSource failedImages
    putStrLn $ "generateErrorReportText: Filtered log length: " ++ show (length filteredLog) ++ " characters"

    -- Build the text error report
    let errorTextContent = "LaTeX Compilation Error Report\n" ++
                          "================================\n\n" ++
                          "Original Document: " ++ originalFileName ++ "\n\n" ++
                          (if not (null failedImages) then
                             "Failed Image Downloads:\n" ++
                             "----------------------\n" ++
                             concatMap (\img -> "  * " ++ Document.filename img ++ "\n" ++
                                              "    URL: " ++ Document.url img ++ "\n") failedImages ++
                             "\n"
                           else "") ++
                          "Error Log (with source line annotations):\n" ++
                          "========================================\n\n" ++
                          filteredLog

    -- Write the error report to a text file
    let errorTextPath = "outbox/" ++ errorTextFileName
    writeFile errorTextPath errorTextContent
    putStrLn $ "generateErrorReportText: Wrote error log to " ++ errorTextPath

    -- Build JSON error records
    let latexLines = lines latexSource
        errorRecords = mapMaybe (buildErrorRecord latexLines concordanceMap) errorLineNumbers
        -- Remove duplicates - keep only first item with each scripta-line
        uniqueErrorRecords = nubBy (\a b -> scriptaLine a == scriptaLine b) errorRecords

    -- Write JSON to save/error.json with each element on a single line
    let jsonPath = "save/error.json"
        -- Format as compact JSON with each object on one line
        formattedJson = if null uniqueErrorRecords
                        then "[]"
                        else "[\n" ++
                             BLC.unpack (BLC.intercalate ",\n" (map encode uniqueErrorRecords)) ++
                             "\n]"
    writeFile jsonPath formattedJson
    putStrLn $ "generateErrorReportText: Wrote " ++ show (length uniqueErrorRecords) ++ " unique error records to " ++ jsonPath

    return ()
  where
    buildErrorRecord :: [String] -> Map.Map Int Concordance.ErrorLines -> Int -> Maybe ErrorRecord
    buildErrorRecord latexLines concordanceMap latexLineNum =
        case Map.lookup latexLineNum concordanceMap of
            Just entry ->
                let latexText = if latexLineNum > 0 && latexLineNum <= length latexLines
                                then latexLines !! (latexLineNum - 1)
                                else ""
                in Just $ ErrorRecord
                    { scriptaLine = Concordance.scriptaSrc entry
                    , latexLine = latexLineNum
                    , latexBegin = Concordance.begin entry
                    , latexEnd = Concordance.end entry
                    , latexText = latexText
                    }
            Nothing -> Nothing

readLogFile :: String -> IO String
readLogFile logPath = do
    result <- try (readFile logPath) :: IO (Either IOError String)
    case result of
        Left _ -> return "Error: Could not read log file"
        Right content -> return content
  where
    try :: IO a -> IO (Either IOError a)
    try action = catch (Right <$> action) (return . Left)

-- Escape special LaTeX characters in URLs
escapeLatex :: String -> String
escapeLatex [] = []
escapeLatex (c:cs) = case c of
    '\\' -> "\\textbackslash{}" ++ escapeLatex cs
    '{' -> "\\{" ++ escapeLatex cs
    '}' -> "\\}" ++ escapeLatex cs
    '$' -> "\\$" ++ escapeLatex cs
    '&' -> "\\&" ++ escapeLatex cs
    '#' -> "\\#" ++ escapeLatex cs
    '^' -> "\\^{}" ++ escapeLatex cs
    '_' -> "\\_" ++ escapeLatex cs
    '~' -> "\\~{}" ++ escapeLatex cs
    '%' -> "\\%" ++ escapeLatex cs
    _ -> c : escapeLatex cs

-- Extract line number from "l.NNN" pattern
extractLineNumber :: String -> Maybe Int
extractLineNumber line =
    case dropWhile (/= 'l') line of
        ('l':'.':rest) -> readMaybe (takeWhile isDigit rest)
        _ -> Nothing

-- Extract LaTeX text from "l.NNN text..." pattern
extractLatexText :: String -> String
extractLatexText line =
    case dropWhile (/= 'l') line of
        ('l':'.':rest) ->
            let afterNum = dropWhile isDigit rest
            in case afterNum of
                (' ':text) -> text
                _ -> ""
        _ -> ""

-- Build concordance map for quick lookup
buildConcordanceMap :: String -> String -> Map.Map Int Concordance.ErrorLines
buildConcordanceMap logContent latexSource =
    let entries = Concordance.buildConcordance logContent latexSource
    in Map.fromList [(Concordance.latexSrc e, e) | e <- entries]

-- Find failed images mentioned in the LaTeX log
findFailedImagesInLog :: [String] -> [ImageElement] -> [ImageElement]
findFailedImagesInLog logLines allFailedImages =
    filter (\img -> any (isImageError (Document.filename img)) logLines) allFailedImages
  where
    isImageError fname line =
        ("Unable to load" `isInfixOf` line && fname `isInfixOf` line) ||
        ("image/" ++ fname) `isInfixOf` line && "!" `isInfixOf` line

-- Filter LaTeX log to extract only error messages, with URL annotations for images and concordance info
filterLatexLogWithUrls :: String -> String -> [ImageElement] -> String
filterLatexLogWithUrls logContent latexSource failedImages =
    let logLines = lines logContent
        -- Build concordance map
        concordanceMap = buildConcordanceMap logContent latexSource
        _ = unsafePerformIO $ putStrLn $ "filterLatexLogWithUrls: Processing " ++ show (length logLines) ++ " log lines"
        _ = unsafePerformIO $ putStrLn $ "filterLatexLogWithUrls: Concordance map has " ++ show (Map.size concordanceMap) ++ " entries"
        _ = unsafePerformIO $ putStrLn $ "filterLatexLogWithUrls: Concordance keys: " ++ show (Map.keys concordanceMap)

        -- Filter out font loading lines and other boilerplate
        isBoilerplateLine line = any (`isInfixOf` line)
            [ "LaTeX Font Info:"
            , "Font shape"
            , "(/usr/local/texlive"
            , "/texmf-dist/fonts/"
            , "size <"
            , "external font"
            , "Font Info:"
            , ".fd"
            , ".tfm"
            , ".pfb"
            , ".otf"
            , ".ttf"
            , "umsa.fd"
            , "umsb.fd"
            , "Trying to load font"
            , "LaTeX2e <"
            , "Document Class:"
            , "Package:"
            , "File:"
            , "Loading"
            , "(Font)"
            , "Redeclaring"
            , "For additional information"
            , "ABD:"
            , "Using"
            , "Overwriting"
            , "********"
            ]
        cleanedLines = filter (not . isBoilerplateLine) logLines
        -- Patterns that indicate actual errors or important messages
        isErrorLine line = any (`isInfixOf` line)
            [ "! LaTeX Error:"
            , "! Emergency stop"
            , "! Undefined control sequence"
            , "! Missing"
            , "! Package"
            , "! Too many"
            , "! Extra"
            , "! Paragraph ended"
            , "! File ended"
            , "! Illegal"
            , "! Division by 0"
            , "! Unable to load"
            , "! Cannot"
            , "! I can't"
            , "runaway"
            , "Fatal error"
            , "Error:"
            , "Warning:"
            , "l." -- Line number indicators for errors
            , "..." -- Context lines
            , "<*>"
            ]

        -- Also include a few lines after each error for context
        -- Look ahead in context lines for line number references
        extractErrors :: [String] -> [String]
        extractErrors [] = []
        extractErrors (line:rest)
            | isErrorLine line =
                let contextLines = take 3 rest
                    -- Remove trailing blank/whitespace-only lines from context
                    isBlank s = all (\c -> c == ' ' || c == '\t') s
                    trimmedContext = reverse $ dropWhile isBlank $ reverse contextLines
                    -- Insert blank line before line number references (lines starting with "l.")
                    insertBlankBeforeLineNum [] = []
                    insertBlankBeforeLineNum (l:ls)
                        | "l." `isPrefixOf` l = "" : l : insertBlankBeforeLineNum ls
                        | otherwise = l : insertBlankBeforeLineNum ls
                    contextWithBlanks = insertBlankBeforeLineNum trimmedContext
                    -- Look for line number in current line or context
                    lineNum = case extractLineNumber line of
                        Just n -> Just n
                        Nothing -> listToMaybe $ mapMaybe extractLineNumber contextLines
                    -- Build annotation as a separate line with [par] marker
                    annotation = case lineNum of
                        Just n -> case Map.lookup n concordanceMap of
                            Just entry ->
                                ["[par]",
                                 "[scriptaErrorAt " ++ show (Concordance.scriptaSrc entry) ++
                                 " >>> Scripta text at line " ++ show (Concordance.scriptaSrc entry) ++
                                 " produced the above error (" ++ show (Concordance.begin entry) ++
                                 ", " ++ show (Concordance.end entry) ++ ")]",
                                 ""]
                            Nothing ->
                                ["[par]",
                                 "[scriptaErrorAt " ++ show n ++ " >>> Error at LaTeX line " ++ show n ++
                                 " (in document preamble, no source mapping available)]",
                                 ""]
                        Nothing -> []
                in line : contextWithBlanks ++ annotation ++ extractErrors (drop 3 rest)
            | otherwise = extractErrors rest

        -- Apply filtering to cleaned lines
        filteredLines = extractErrors cleanedLines
    in if null filteredLines
       then "No specific errors found in log. Check full log for details."
       else unlines filteredLines

-- Original filter without URL annotations (kept for compatibility)
filterLatexLog :: String -> String
filterLatexLog logContent = filterLatexLogWithUrls logContent "" []

-- Create PDF with filtered error messages (cleaner version)
createWithFilteredErrors :: Document -> [ImageElement] -> IO Text
createWithFilteredErrors document failedImages = do
    putStrLn "createWithFilteredErrors: Starting..."
    resultOrError <- catch (fmap Right (create document)) $ \e -> do
        putStrLn $ "createWithFilteredErrors: Caught exception: " ++ show (e :: IOError)
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
            putStrLn $ "createWithFilteredErrors: Got result: " ++ show result
            case result of
                PdfSuccess fname -> return fname
                PdfWithErrors pdfFile errorPdfFile errMsg -> do
                    -- When we have both files, return the error PDF filename
                    -- The client can choose which one to display
                    putStrLn $ "createWithFilteredErrors: Both PDF and error report available"
                    return errorPdfFile
                PdfError errMsg logContent -> do
                    putStrLn "createWithFilteredErrors: Handling error case..."
                    let fileName = unpack $ Document.docId document
                        pdfFileName = replace ".tex" ".pdf" fileName
                        errorTexFileName = "inbox/error-" ++ fileName
                        -- Check if it was a timeout
                        isTimeout = "timed out" `isInfixOf` (unpack errMsg)
                        -- Get all images from the document for URL mapping
                        allImages = urlList document
                        latexSourcePath = "inbox/" ++ fileName
                    -- Read the LaTeX source for concordance
                    putStrLn $ "createWithFilteredErrors: Reading LaTeX source from: " ++ latexSourcePath
                    latexSource <- catch (readFile latexSourcePath) $ \(e :: IOError) -> do
                        putStrLn $ "createWithFilteredErrors: Failed to read LaTeX source: " ++ show e
                        return ""
                    putStrLn $ "createWithFilteredErrors: Read " ++ show (length latexSource) ++ " bytes"
                    let concordanceMap = buildConcordanceMap (unpack logContent) latexSource
                    putStrLn $ "createWithFilteredErrors: Built concordance with " ++ show (Map.size concordanceMap) ++ " entries"
                    let -- Filter the error log to show only relevant errors, with URL annotations for ALL images
                        filteredLog = filterLatexLogWithUrls (unpack logContent) latexSource allImages
                        -- Look for specific errors in the log
                        logLines = lines (unpack logContent)
                        hasImageError = not isTimeout && any (\line -> "Unable to load picture" `isInfixOf` line || "! Unable to load" `isInfixOf` line) logLines
                        hasBraceError = not isTimeout && any (\line -> "Paragraph ended before" `isInfixOf` line || "Too many }" `isInfixOf` line) logLines
                        hasDivisionError = not isTimeout && any (\line -> "Division by 0" `isInfixOf` line) logLines
                        -- Find which images failed in the LaTeX log and map them to URLs
                        failedInLatex = findFailedImagesInLog logLines failedImages
                        -- Create specific error messages based on what we found
                        specificErrors = if isTimeout then
                                           "\\item \\textbf{Compilation Timeout:} The document took more than 30 seconds to compile and was terminated.\n"
                                         else if hasImageError || not (null failedImages) then
                                           "\\item \\textbf{Missing Images:} Some images could not be loaded. See details below.\n"
                                         else "" ++
                                         if hasBraceError then
                                           "\\item \\textbf{Unmatched Braces:} The document has mismatched \\{ \\} braces.\n"
                                         else "" ++
                                         if hasDivisionError then
                                           "\\item \\textbf{Division by Zero:} An image dimension calculation resulted in division by zero.\n"
                                         else ""
                        -- Add failed images section if there are any
                        failedImagesSection = if not (null failedImages) then
                                                "\\section{Failed Image Downloads}\n" ++
                                                "The following images could not be downloaded:\n\n" ++
                                                "\\begin{itemize}\n" ++
                                                concatMap (\img -> "\\item \\textbf{" ++ Document.filename img ++ "}\\\\\n" ++
                                                                  "\\quad URL: \\texttt{" ++ escapeLatex (take 60 (Document.url img)) ++
                                                                  (if length (Document.url img) > 60 then "..." else "") ++ "}\n") failedImages ++
                                                "\\end{itemize}\n\n"
                                              else ""
                        errorTexContent = "\\documentclass{article}\n" ++
                                          "\\usepackage{geometry}\n" ++
                                          "\\geometry{letterpaper, margin=1in}\n" ++
                                          "\\begin{document}\n" ++
                                          "\\title{LaTeX Compilation Error Report}\n" ++
                                          "\\date{\\today}\n" ++
                                          "\\maketitle\n" ++
                                          "\\section{Error Summary}\n" ++
                                          (if isTimeout then "The document compilation was terminated after exceeding the 30-second time limit.\n\n"
                                           else "The LaTeX compilation encountered errors that prevented PDF generation.\n\n") ++
                                          (if not (null specificErrors) then
                                             "\\subsection{Issues Found}\n" ++
                                             "\\begin{itemize}\n" ++
                                             specificErrors ++
                                             "\\end{itemize}\n\n"
                                           else "") ++
                                          failedImagesSection ++
                                          (if isTimeout then
                                             "\\section{Troubleshooting Timeout Issues}\n" ++
                                             "\\begin{itemize}\n" ++
                                             "\\item Check for infinite loops in macro definitions\n" ++
                                             "\\item Simplify complex TikZ or PGF graphics\n" ++
                                             "\\item Verify no recursive \\\\input or \\\\include commands\n" ++
                                             "\\item Break up very large documents\n" ++
                                             "\\end{itemize}\n\n"
                                           else
                                             "\\section{Error Details}\n" ++
                                             "{\\small\n" ++
                                             "\\begin{verbatim}\n" ++
                                             filteredLog ++ "\n" ++
                                             "\\end{verbatim}\n" ++
                                             "}\n\n" ++
                                             "\\section{Common Solutions}\n" ++
                                             "\\begin{itemize}\n" ++
                                             "\\item Check for typos in LaTeX commands\n" ++
                                             "\\item Ensure all braces \\{ \\} are matched\n" ++
                                             "\\item Verify all required packages are included\n" ++
                                             "\\item Check that image files exist and are accessible\n" ++
                                             "\\end{itemize}\n") ++
                                          "\\end{document}"
                    -- Write the error TeX file
                    putStrLn $ "createWithFilteredErrors: Writing error tex file: " ++ errorTexFileName
                    writeFile errorTexFileName errorTexContent
                    -- Compile it to PDF
                    putStrLn "createWithFilteredErrors: Compiling error PDF..."
                    exitCode <- system $ "xelatex -output-directory=outbox -interaction=nonstopmode " ++ errorTexFileName ++ " >/dev/null 2>&1"
                    putStrLn $ "createWithFilteredErrors: XeLaTeX exit code for error PDF: " ++ show exitCode
                    -- Clean up the tex file
                    system ("rm -f " ++ errorTexFileName ++ " 2>/dev/null") >>= \_ -> return ()
                    case exitCode of
                        ExitSuccess -> do
                            putStrLn "createWithFilteredErrors: Error PDF created successfully"
                            -- Copy the error PDF to the expected filename
                            let errorPdfFileName = replace ".tex" ".pdf" ("error-" ++ fileName)
                            system $ "cp outbox/" ++ errorPdfFileName ++ " outbox/" ++ pdfFileName ++ " 2>/dev/null"
                            return (pack pdfFileName)
                        ExitFailure _ -> do
                            putStrLn "createWithFilteredErrors: Error PDF compilation failed, creating fallback"
                            -- Create minimal fallback PDF
                            let fallbackContent = "\\documentclass{article}\n\\begin{document}\n" ++
                                                "\\title{Document Processing Error}\n\\maketitle\n" ++
                                                "LaTeX compilation failed. Please check your document for errors.\n" ++
                                                "\\end{document}"
                            writeFile errorTexFileName fallbackContent
                            system $ "pdflatex -output-directory=outbox -interaction=nonstopmode " ++ errorTexFileName ++ " >/dev/null 2>&1"
                            system ("rm -f " ++ errorTexFileName ++ " 2>/dev/null") >>= \_ -> return ()
                            let minimalPdfFileName = replace ".tex" ".pdf" ("error-" ++ fileName)
                            system $ "cp outbox/" ++ minimalPdfFileName ++ " outbox/" ++ pdfFileName ++ " 2>/dev/null"
                            return (pack pdfFileName)

-- Wrapper for backward compatibility
createWithErrorPdf :: Document -> IO Text
createWithErrorPdf document = createWithFailedImages document []


