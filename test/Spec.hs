module Main where

import Test.HUnit
import System.Exit (exitFailure, exitSuccess)
import Cache (hashInputs, cacheLookupIn, cacheStoreIn)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import qualified Control.Exception

hashTests :: Test
hashTests = TestList
  [ "golden vector" ~:
      hashInputs "x" ["u"] ["p"] ~?= "0c05785f1721e70967478bfc56c9c63bec85e8f9b8eb406072bc66cdafe61bac"
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
  , "section separator prevents collision" ~:
      assertBool "ab+[] must differ from a+[b]"
        (hashInputs "ab" [] [] /= hashInputs "a" ["b"] [])
  , "output is lowercase hex" ~:
      assertBool "only 0-9a-f"
        (all (`elem` "0123456789abcdef") (hashInputs "x" [] []))
  ]

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

  , "store with absent source is a no-op" ~: TestCase $ do
      let dir = "test-cache-tmp3"
      removeDirectoryRecursive dir `catchAny` \_ -> return ()
      createDirectoryIfMissing True dir
      cacheStoreIn dir "ghost" (dir ++ "/does-not-exist.pdf")
      hit <- cacheLookupIn dir "ghost" (dir ++ "/dest.pdf")
      assertBool "absent source must not populate the cache" (not hit)
      removeDirectoryRecursive dir

  , "copy failure on hit falls through to a miss" ~: TestCase $ do
      let dir = "test-cache-tmp4"
      removeDirectoryRecursive dir `catchAny` \_ -> return ()
      createDirectoryIfMissing True dir
      writeFile (dir ++ "/src.pdf") "DATA"
      cacheStoreIn dir "abc" (dir ++ "/src.pdf")
      hit <- cacheLookupIn dir "abc" (dir ++ "/no-such-subdir/dest.pdf")
      assertBool "copy failure must be treated as a miss" (not hit)
      removeDirectoryRecursive dir
  ]
  where
    catchAny :: IO a -> (IOError -> IO a) -> IO a
    catchAny = Control.Exception.catch

main :: IO ()
main = do
  counts <- runTestTT (TestList [hashTests, ioTests])
  if errors counts + failures counts == 0 then exitSuccess else exitFailure
