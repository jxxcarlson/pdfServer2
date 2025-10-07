module Concordance
    ( ErrorLines(..)
    , buildConcordance
    , formatConcordanceCSV
    ) where

import qualified Data.Map.Strict as Map
import Data.List (sort, nub, isPrefixOf)
import Data.Maybe (mapMaybe)
import Data.Char (isDigit, isSpace)
import Text.Read (readMaybe)

-- Define a record type for error line information
data ErrorLines = ErrorLines
  { latexSrc :: Int
  , begin :: Int
  , end :: Int
  , scriptaSrc :: Int
  } deriving (Show, Eq)

-- Extract number after "l." pattern
extractNumber :: String -> Maybe Int
extractNumber ('l':'.':rest) =
    let numStr = takeWhile isDigit rest
    in if null numStr then Nothing else readMaybe numStr
extractNumber (_:rest) = extractNumber rest
extractNumber [] = Nothing

-- Extract all line numbers from "l.NNN" pattern in error log
extractErrorLines :: String -> [Int]
extractErrorLines content =
    let contentLines = lines content
        extractFromLine line = extractNumber line
    in nub $ sort $ mapMaybe extractFromLine contentLines

-- Extract number after "%%% Line " pattern
extractLineAnnotation :: String -> Maybe Int
extractLineAnnotation line
    | "%%% Line " `isPrefixOf` line =
        let numStr = drop 9 line  -- drop "%%% Line "
        in readMaybe $ takeWhile isDigit numStr
    | otherwise = Nothing

-- Parse entropy.tex to extract %%% Line annotations with their LaTeX line numbers
extractLineMapping :: String -> [(Int, Int)]  -- [(latexLine, scriptaLine)]
extractLineMapping content =
    let linesWithNumbers = zip [1..] (lines content)
        parseLineComment (latexLine, line) =
            case extractLineAnnotation line of
                Just scriptaLine -> Just (latexLine, scriptaLine)
                Nothing -> Nothing
    in mapMaybe parseLineComment linesWithNumbers

-- Check if a line is blank (empty or only whitespace)
isBlankLine :: String -> Bool
isBlankLine = all isSpace

-- Find paragraph boundaries for each line
findParagraphBoundaries :: [String] -> [(Int, Int, Int)]  -- [(lineNum, begin, end)]
findParagraphBoundaries content =
    let linesWithNumbers = zip [1..] content
        isAnnotation line = "%%% Line " `isPrefixOf` line

        boundaries = [n | (n, line) <- linesWithNumbers, isBlankLine line || isAnnotation line]

        findBounds lineNum =
            let precedingBounds = filter (<= lineNum) boundaries
                followingBounds = filter (> lineNum) boundaries
                paragraphStart = case precedingBounds of
                    [] -> 1
                    bs -> last bs
                paragraphEnd = case followingBounds of
                    [] -> length content
                    (b:_) -> b - 1
            in (lineNum, paragraphStart, paragraphEnd)
    in map (findBounds . fst) linesWithNumbers

-- Build concordance from error log and LaTeX source
buildConcordance :: String -> String -> [ErrorLines]
buildConcordance errorLogContent latexSourceContent =
    let errorLines = extractErrorLines errorLogContent
        lineMapping = extractLineMapping latexSourceContent
        latexLines = lines latexSourceContent
        sortedMapping = sort lineMapping
        boundaries = findParagraphBoundaries latexLines
        boundaryMap = Map.fromList [(ln, (b, e)) | (ln, b, e) <- boundaries]

        findErrorLineInfo errorLine =
            let preceding = filter (\(latexLine, _) -> latexLine <= errorLine) sortedMapping
                (paragraphBegin, paragraphEnd) = Map.findWithDefault (errorLine, errorLine) errorLine boundaryMap
            in case preceding of
                [] -> Nothing
                _ -> let (_, scriptaLine) = last preceding
                     in Just $ ErrorLines errorLine paragraphBegin paragraphEnd scriptaLine
    in mapMaybe findErrorLineInfo errorLines

-- Format concordance for CSV output
formatConcordanceCSV :: [ErrorLines] -> String
formatConcordanceCSV records =
    let header = "latexSrc, begin, end, scriptaSrc"
        formatEntry (ErrorLines latex begin end scripta) =
            show latex ++ ", " ++ show begin ++ ", " ++ show end ++ ", " ++ show scripta
    in unlines (header : map formatEntry records)