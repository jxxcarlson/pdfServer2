{-# LANGUAGE OverloadedStrings #-}

module Cache
  ( cacheDir
  , hashInputs
  , contentHash
  , cacheLookup
  , cacheStore
  , cacheLookupIn
  , cacheStoreIn
  , pruneCache
  ) where

import Crypto.Hash (hashWith, Digest, SHA256(..))
import qualified Data.Text as TS
import Data.Text.Encoding (encodeUtf8)
import Data.List (intercalate)
import qualified Data.Text.Lazy as TL
import Document (Document, content, urlList, packageList, url)
import System.Directory (doesFileExist, copyFile, createDirectoryIfMissing)
import System.Process (system)
import System.FilePath ((</>))
import Control.Exception (try, IOException)

-- Where cached PDFs live (relative to the server working directory).
cacheDir :: FilePath
cacheDir = "pdfcache"

-- SHA-256 (hex) over the inputs that fully determine a rendered PDF: the LaTeX
-- body, the referenced image URLs, and the package list. Sections are NUL-joined
-- so that, e.g., content "a" + url "b" cannot collide with content "ab" + no url.
hashInputs :: String -> [String] -> [String] -> String
hashInputs body urls packages =
  let blob = intercalate "\NUL" (body : urls ++ packages)
      digest = hashWith SHA256 (encodeUtf8 (TS.pack blob)) :: Digest SHA256
  in show digest

-- Adapter: derive the hash from a decoded Document.
contentHash :: Document -> String
contentHash doc =
  hashInputs
    (TL.unpack (content doc))
    (map url (urlList doc))
    (map TL.unpack (packageList doc))

-- Maximum age (days) a cached PDF is kept before it is recompiled.
cacheTtlDays :: Int
cacheTtlDays = 7

cachePath :: FilePath -> String -> FilePath
cachePath dir hash = dir </> (hash ++ ".pdf")

-- On a hit, copy the cached PDF to destPath and refresh its mtime so actively
-- used documents stay warm; return whether it was a hit.
cacheLookupIn :: FilePath -> String -> FilePath -> IO Bool
cacheLookupIn dir hash destPath = do
  let cached = cachePath dir hash
  exists <- doesFileExist cached
  if exists
    then do
      result <- try (copyFile cached destPath) :: IO (Either IOException ())
      case result of
        Left _   -> return False
        Right () -> do
          _ <- system ("touch '" ++ cached ++ "' 2>/dev/null || true")
          return True
    else return False

-- Copy a freshly produced PDF into the cache under its hash (creating the dir).
cacheStoreIn :: FilePath -> String -> FilePath -> IO ()
cacheStoreIn dir hash srcPath = do
  createDirectoryIfMissing True dir
  exists <- doesFileExist srcPath
  if exists then copyFile srcPath (cachePath dir hash) else return ()

cacheLookup :: String -> FilePath -> IO Bool
cacheLookup = cacheLookupIn cacheDir

cacheStore :: String -> FilePath -> IO ()
cacheStore = cacheStoreIn cacheDir

-- Delete cache entries older than cacheTtlDays. Safe if the dir does not exist.
pruneCache :: IO ()
pruneCache = do
  _ <- system ("find '" ++ cacheDir ++ "' -type f -mtime +" ++ show cacheTtlDays
               ++ " -delete 2>/dev/null || true")
  return ()
