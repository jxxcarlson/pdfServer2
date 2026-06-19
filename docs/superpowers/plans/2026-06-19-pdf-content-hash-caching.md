# Content-hash PDF Caching Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Skip image download and all xelatex passes on `POST /pdf` when an identical document was compiled recently, by caching the result PDF under a content hash.

**Architecture:** A new `Cache` library module computes `SHA-256(content + urlList + packageList)` and reads/writes `pdfcache/<hash>.pdf`. The `/pdf` handler in `Main.hs` checks the cache first: on a hit it copies the cached PDF into `outbox/` under the document's normal name and returns immediately; on a miss it compiles as today and stores the result on success.

**Tech Stack:** Haskell, Scotty, stack (resolver `lts-22.44`, GHC 9.6.3/9.6.7), `crypton` for SHA-256, `HUnit` for tests.

## Global Constraints

- Cache key = `SHA-256(content ++ urlList-urls ++ packageList)`, hex-encoded (64 lowercase hex chars), computed via `crypton`'s `Crypto.Hash`.
- Cache directory = `pdfcache/` (relative to the server working dir), files named `<hash>.pdf`.
- Eviction = time-only, `find pdfcache -type f -mtime +7 -delete`. No size cap.
- Cache **only** `PdfSuccess`. Never cache `PdfWithErrors` or `PdfError`.
- Only the `POST /pdf` endpoint uses the cache. `/json` and `/tex` are unchanged.
- No cross-request locking, no metrics, no error caching (deferred — see memory `project_pdfcache_yagni`).
- `content` and `packageList` are **lazy** `Data.Text.Lazy.Text`; `ImageElement.url :: String`.

---

## File Structure

- `package.yaml` — add `crypton` to library `dependencies`; add `HUnit` to test `dependencies`.
- `src/Document.hs` — add `packageList` to the module export list (already defined, not exported).
- `src/Cache.hs` — **new** library module: pure hashing (`hashInputs`, `contentHash`) and file cache I/O (`cacheLookup`, `cacheStore`, `pruneCache`, plus directory-parameterized `cacheLookupIn`/`cacheStoreIn` for tests).
- `app/Main.hs` — modify the `post "/pdf"` handler (lines 76–94) to consult the cache.
- `test/Spec.hs` — replace the stub with an HUnit suite covering `Cache`.

---

### Task 1: Cache module — pure content hashing

**Files:**
- Modify: `package.yaml` (library `dependencies`: add `crypton`; test `dependencies`: add `HUnit`)
- Modify: `src/Document.hs:3` (export list — add `packageList`)
- Create: `src/Cache.hs`
- Create/replace: `test/Spec.hs`

**Interfaces:**
- Produces:
  - `Cache.hashInputs :: String -> [String] -> [String] -> String` — hex SHA-256 of (content, urls, packages); 64 lowercase hex chars.
  - `Cache.contentHash :: Document.Document -> String` — adapter that pulls `content`/`urlList`/`packageList` out of a `Document` and calls `hashInputs`.
  - `Cache.cacheDir :: FilePath` — `"pdfcache"`.

- [ ] **Step 1: Add dependencies**

In `package.yaml`, under the top-level `library:` block add `crypton` to its `dependencies` (the library block currently has no `dependencies:` key — add one):

```yaml
library:
  source-dirs: src
  dependencies:
  - crypton
```

Under `tests:` → `pdfServerScotty-test:` → `dependencies:` add `HUnit`:

```yaml
  pdfServerScotty-test:
    main:                Spec.hs
    source-dirs:         test
    ghc-options:
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    dependencies:
    - pdfServerScotty
    - HUnit
```

- [ ] **Step 2: Export `packageList` from Document**

In `src/Document.hs`, change the export list on line 3 from:

```haskell
module Document (Document, ImageElement(..), fixGraphicsPath, docId, content, prepareData, urlList) where
```

to (add `packageList`):

```haskell
module Document (Document, ImageElement(..), fixGraphicsPath, docId, content, prepareData, urlList, packageList) where
```

- [ ] **Step 3: Write the failing test for `hashInputs`**

Replace the entire contents of `test/Spec.hs` with:

```haskell
module Main where

import Test.HUnit
import System.Exit (exitFailure, exitSuccess)
import Cache (hashInputs)

hashTests :: Test
hashTests = TestList
  [ "deterministic" ~:
      hashInputs "x" ["u"] ["p"] ~?= hashInputs "x" ["u"] ["p"]
  , "hex length is 64" ~:
      length (hashInputs "x" [] []) ~?= 64
  , "content-sensitive" ~:
      assertBool "different content -> different hash"
        (hashInputs "x" [] [] /= hashInputs "y" [] [])
  , "url-sensitive" ~:
      assertBool "different url -> different hash"
        (hashInputs "x" ["u1"] [] /= hashInputs "x" ["u2"] [])
  , "package-sensitive" ~:
      assertBool "different package -> different hash"
        (hashInputs "x" [] ["p1"] /= hashInputs "x" [] ["p2"])
  ]

main :: IO ()
main = do
  counts <- runTestTT hashTests
  if errors counts + failures counts == 0 then exitSuccess else exitFailure
```

- [ ] **Step 4: Run the test to verify it fails**

Run: `stack test`
Expected: FAIL — compilation error, `Cache` module / `hashInputs` not found.

- [ ] **Step 5: Implement `Cache.hs` (pure part)**

Create `src/Cache.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Cache
  ( cacheDir
  , hashInputs
  , contentHash
  ) where

import Crypto.Hash (hashWith, Digest, SHA256(..))
import qualified Data.Text as TS
import Data.Text.Encoding (encodeUtf8)
import Data.List (intercalate)
import qualified Data.Text.Lazy as TL
import Document (Document, content, urlList, packageList, url)

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
```

Note: if `import Crypto.Hash (... SHA256(..))` fails to find `SHA256`, change it to
`import Crypto.Hash (hashWith, Digest)` plus `import Crypto.Hash.Algorithms (SHA256(..))`.

- [ ] **Step 6: Run the test to verify it passes**

Run: `stack test`
Expected: PASS — `Cases: 5  Tried: 5  Errors: 0  Failures: 0`.

- [ ] **Step 7: Commit**

```bash
git add package.yaml src/Document.hs src/Cache.hs test/Spec.hs
git commit -m "feat(cache): content-hash function for PDF caching"
```

---

### Task 2: Cache module — file lookup, store, prune

**Files:**
- Modify: `src/Cache.hs`
- Modify: `test/Spec.hs`

**Interfaces:**
- Consumes: `Cache.cacheDir` (Task 1).
- Produces:
  - `Cache.cacheLookupIn :: FilePath -> String -> FilePath -> IO Bool` — (cacheDir, hash, destPath). On hit: copy `cacheDir/<hash>.pdf` to `destPath`, refresh its mtime, return `True`. On miss: return `False`, copy nothing.
  - `Cache.cacheStoreIn :: FilePath -> String -> FilePath -> IO ()` — (cacheDir, hash, srcPath). Create the dir if needed; copy `srcPath` to `cacheDir/<hash>.pdf` (no-op if `srcPath` is absent).
  - `Cache.cacheLookup :: String -> FilePath -> IO Bool` = `cacheLookupIn cacheDir`.
  - `Cache.cacheStore :: String -> FilePath -> IO ()` = `cacheStoreIn cacheDir`.
  - `Cache.pruneCache :: IO ()` — delete cache files older than 7 days.

- [ ] **Step 1: Write the failing tests for store/lookup**

In `test/Spec.hs`, add imports at the top (below the existing imports):

```haskell
import Cache (cacheLookupIn, cacheStoreIn)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
```

Add this test group:

```haskell
ioTests :: Test
ioTests = TestList
  [ "store then lookup round-trips and copies bytes" ~: TestCase $ do
      let dir = "test-cache-tmp"
      removeDirectoryRecursive dir `catchAny` \_ -> return ()
      createDirectoryIfMissing True dir
      writeFile (dir ++ "/src.pdf") "PDFDATA"
      cacheStoreIn dir "deadbeef" (dir ++ "/src.pdf")
      hit <- cacheLookupIn dir "deadbeef" (dir ++ "/dest.pdf")
      assertBool "expected a hit" hit
      out <- readFile (dir ++ "/dest.pdf")
      assertEqual "cached bytes" "PDFDATA" out
      removeDirectoryRecursive dir

  , "lookup of unknown hash is a miss" ~: TestCase $ do
      let dir = "test-cache-tmp2"
      removeDirectoryRecursive dir `catchAny` \_ -> return ()
      createDirectoryIfMissing True dir
      hit <- cacheLookupIn dir "nope" (dir ++ "/dest.pdf")
      assertBool "expected a miss" (not hit)
      removeDirectoryRecursive dir
  ]
  where
    catchAny :: IO a -> (IOError -> IO a) -> IO a
    catchAny = Control.Exception.catch
```

Add this import for `catchAny` near the top of `test/Spec.hs`:

```haskell
import qualified Control.Exception
```

And change `main` to run both groups:

```haskell
main :: IO ()
main = do
  counts <- runTestTT (TestList [hashTests, ioTests])
  if errors counts + failures counts == 0 then exitSuccess else exitFailure
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `stack test`
Expected: FAIL — `cacheLookupIn` / `cacheStoreIn` not in scope.

- [ ] **Step 3: Implement the file I/O in `Cache.hs`**

Update the module export list and add imports + functions. New export list:

```haskell
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
```

Add these imports:

```haskell
import System.Directory (doesFileExist, copyFile, createDirectoryIfMissing)
import System.Process (system)
import System.FilePath ((</>))
```

Add these definitions:

```haskell
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
      copyFile cached destPath
      _ <- system ("touch " ++ cached ++ " 2>/dev/null || true")
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
  _ <- system ("find " ++ cacheDir ++ " -type f -mtime +" ++ show cacheTtlDays
               ++ " -delete 2>/dev/null || true")
  return ()
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `stack test`
Expected: PASS — `Cases: 7  Tried: 7  Errors: 0  Failures: 0`.

- [ ] **Step 5: Commit**

```bash
git add src/Cache.hs test/Spec.hs
git commit -m "feat(cache): file lookup/store/prune for PDF cache"
```

---

### Task 3: Wire the cache into the `/pdf` handler

**Files:**
- Modify: `app/Main.hs` (imports near line 22–23; the `post "/pdf"` handler, lines 76–94)

**Interfaces:**
- Consumes: `Cache.contentHash`, `Cache.cacheLookup`, `Cache.cacheStore`, `Cache.pruneCache` (Tasks 1–2); `Document.docId`, `Document.prepareData`, `Pdf.create`, `PdfResult(..)` (existing).

- [ ] **Step 1: Add imports**

In `app/Main.hs`, add to the imports (after the existing `import Document (...)` on line 23):

```haskell
import qualified Cache
import Data.Text.Lazy (unpack, replace)
```

(If `Data.Text.Lazy (pack, unpack, replace, toLower, Text)` is already imported in this file, do **not** add a duplicate import — `unpack` and `replace` are already in scope. Verify before editing.)

- [ ] **Step 2: Replace the `/pdf` handler body**

Replace lines 76–94 (the whole `post "/pdf"` block) with:

```haskell
    post "/pdf" $ do
        document <- jsonData :: ActionM Document
        let hash = Cache.contentHash document
        let pdfName = unpack (replace ".tex" ".pdf" (docId document))
        liftIO Cache.pruneCache
        hit <- liftIO $ Cache.cacheLookup hash ("outbox/" ++ pdfName)
        if hit
            then json $ object ["pdf" .= pdfName, "hasErrors" .= False]
            else do
                failedImages <- liftIO $ Document.prepareData document
                result <- liftIO $ Pdf.create document
                case result of
                    PdfSuccess fname -> do
                        liftIO $ Cache.cacheStore hash ("outbox/" ++ unpack fname)
                        json $ object ["pdf" .= fname, "hasErrors" .= False]
                    PdfWithErrors pdfFile errorPdfFile errorJsonFile _ ->
                        json $ object ["pdf" .= pdfFile, "errorReport" .= errorPdfFile, "errorJson" .= errorJsonFile, "hasErrors" .= True]
                    PdfError _ _ -> do
                        pdfFileName <- liftIO $ Pdf.createWithFilteredErrors document failedImages
                        json $ object ["pdf" .= (Nothing :: Maybe Text), "errorReport" .= pdfFileName, "hasErrors" .= True, "pdfFailed" .= True]
```

- [ ] **Step 3: Build**

Run: `stack build`
Expected: compiles with no errors (pre-existing `param` deprecation warnings in `Main.hs` are fine).

- [ ] **Step 4: Run the unit tests (regression)**

Run: `stack test`
Expected: PASS — all 7 cases.

- [ ] **Step 5: Manual end-to-end verification**

There is no automated HTTP harness, so verify by hand. Start the server, then issue the same request twice:

```bash
stack exec pdfServerScotty-exe >/tmp/srv.log 2>&1 &
sleep 2
# Build a request from any .tex in save/ (no images, fast)
python3 - <<'PY'
import json
tex = open("save/aligned-test1.tex").read()
json.dump({"id":"cachetest.tex","title":"cachetest","content":tex,"packageList":[],"urlList":[]}, open("/tmp/req.json","w"))
PY
echo "first (compiles):"
curl -s -m 60 -X POST http://localhost:3000/pdf -H 'Content-Type: application/json' --data @/tmp/req.json -o /dev/null -w 'TOTAL=%{time_total}s http=%{http_code}\n'
echo "second (cache hit, should be much faster):"
curl -s -m 60 -X POST http://localhost:3000/pdf -H 'Content-Type: application/json' --data @/tmp/req.json -o /dev/null -w 'TOTAL=%{time_total}s http=%{http_code}\n'
ls -l pdfcache/   # should contain one <hash>.pdf
pkill -f pdfServerScotty-exe
```

Expected: second request is dramatically faster than the first (no xelatex run), both return `http=200`, and `pdfcache/` contains a `<hash>.pdf`.

- [ ] **Step 6: Commit**

```bash
git add app/Main.hs
git commit -m "feat(cache): serve cached PDF on identical /pdf requests"
```

---

## Self-Review

**1. Spec coverage:**
- Cache key SHA-256(content+urlList+packageList) → Task 1 (`hashInputs`/`contentHash`). ✓
- Dedicated `pdfcache/` dir, `<hash>.pdf` → Task 2 (`cachePath`, `cacheDir`). ✓
- Hit: copy to outbox, touch, return, skip prepareData+create → Task 3. ✓
- Miss: compile, cache only on PdfSuccess → Task 3. ✓
- 7-day mtime eviction → Task 2 (`pruneCache`) + Task 3 (called per request). ✓
- Only `/pdf`; `/json`,`/tex` unchanged → Task 3 touches only the `/pdf` block. ✓
- SHA-256 via crypton → Task 1 deps + `Cache.hs`. ✓
- `Cache` module with `contentHash`/`cacheLookup`/`cacheStore` → Tasks 1–2. ✓
- Error handling: missing dir treated as miss / created on store; copy failures on hit — `cacheStoreIn` creates the dir; `pruneCache` and touch are guarded with `|| true`. (Note: a `copyFile` failure on a hit would throw; acceptable for MVP since the source exists by construction. Not adding a catch — YAGNI.) ✓
- Concurrency: no locking → not implemented (by design). ✓
- Testing: hashInputs determinism/sensitivity + store/lookup round-trip + miss → Tasks 1–2; integration is the manual curl check in Task 3 (no HTTP harness exists). ✓

**2. Placeholder scan:** No TBD/TODO; every code step shows complete code. ✓

**3. Type consistency:** `hashInputs :: String -> [String] -> [String] -> String` used identically in Task 1 impl and tests. `cacheLookupIn`/`cacheStoreIn` signatures match between Task 2 interfaces, impl, and tests. `contentHash`/`cacheLookup`/`cacheStore` names consistent between Task 2 and the Task 3 handler. `pdfName`/`fname` are both the `.pdf` filename (`replace ".tex" ".pdf"`), consistent with `Pdf.create`'s returned `filename`. ✓

---
