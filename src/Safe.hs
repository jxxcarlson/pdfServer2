{-# LANGUAGE ScopedTypeVariables #-}

-- | Input checks for everything that arrives in a request.
--
-- pdfServer used to paste request data (document names, image file names,
-- package names, image URLs) straight into shell commands, so a name like
-- "x; rm -rf ~" ran as a command, and "../../etc/cron.d/x" wrote anywhere.
-- Every such value now goes through this module first:
--
--   * names are reduced to letters, digits, '.', '_' and '-' (anything else
--     becomes '-'), with no leading dot and no "..", so they can't escape
--     their folder or carry shell syntax;
--   * URLs are fetched only if they are plain http(s), and only from public
--     addresses (not localhost, private networks or the cloud metadata
--     service), by running curl directly instead of through a shell.
module Safe
  ( sanitizeName
  , isSafeName
  , fetchUrl
  ) where

import Control.Exception (SomeException, try)
import Data.Bits (shiftR, (.&.))
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.List (isInfixOf, isPrefixOf)
import Data.Word (Word8)
import Network.Socket
  ( AddrInfo (..), Family (..), SockAddr (..), SocketType (..)
  , defaultHints, getAddrInfo, hostAddressToTuple, hostAddress6ToTuple )
import Network.URI (URI (..), URIAuth (..), parseAbsoluteURI)
import System.Directory (doesFileExist, getFileSize)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)


-- NAMES -------------------------------------------------------------------

-- | Make a name safe to use as a file name and inside a shell command.
sanitizeName :: String -> String
sanitizeName raw =
  let mapped = map (\c -> if allowed c then c else '-') raw
      noDotDot = collapseDots mapped
      noLeadingDot = dropWhile (== '.') noDotDot
      trimmed = take 200 noLeadingDot
  in if null trimmed then "document" else trimmed
  where
    allowed c = isAsciiLower c || isAsciiUpper c || isDigit c || c `elem` "._-"
    collapseDots s = if ".." `isInfixOf` s then collapseDots (replaceDots s) else s
    replaceDots ('.' : '.' : rest) = '.' : replaceDots rest
    replaceDots (c : rest) = c : replaceDots rest
    replaceDots [] = []

-- | True when a name is already safe (used for names in GET paths, which
-- are refused rather than rewritten).
isSafeName :: String -> Bool
isSafeName s = not (null s) && sanitizeName s == s


-- URLS --------------------------------------------------------------------

-- | Download a URL to a local file. Returns True when a file larger than
-- 100 bytes was written. Unsafe URLs are not fetched (returns False).
fetchUrl :: String -> FilePath -> IO Bool
fetchUrl url dest = do
  target <- checkUrl url
  case target of
    Left reason -> do
      putStrLn $ "fetchUrl: refused " ++ show (take 120 url) ++ ": " ++ reason
      return False
    Right (host, port, ip) -> do
      let pinned = host ++ ":" ++ show port ++ ":" ++ ip
          args =
            [ "--silent", "--show-error"
            , "--proto", "=http,https"        -- no file://, gopher://, ...
            , "--max-redirs", "0"             -- a redirect could point inward
            , "--max-time", "30"
            , "--max-filesize", "26214400"    -- 25 MB
            , "--resolve", pinned             -- use the address we checked
            , "--output", dest
            , "--", url
            ]
      result <- try (readProcessWithExitCode "curl" args "")
      case result of
        Left (e :: SomeException) -> do
          putStrLn $ "fetchUrl: curl failed to start: " ++ show e
          return False
        Right (ExitSuccess, _, _) -> validDownload dest
        Right (ExitFailure code, _, err) -> do
          putStrLn $ "fetchUrl: curl exit " ++ show code ++ " for " ++ show (take 120 url) ++ ": " ++ take 200 err
          return False

validDownload :: FilePath -> IO Bool
validDownload path = do
  exists <- doesFileExist path
  if exists then (> 100) <$> getFileSize path else return False

-- | Check a URL and resolve its host. On success, returns the host, port
-- and one public IP address to pin curl to.
checkUrl :: String -> IO (Either String (String, Int, String))
checkUrl url
  | not (all urlChar url) = return (Left "characters outside the URL character set")
  | otherwise =
      case parseAbsoluteURI url of
        Nothing -> return (Left "not an absolute URL")
        Just uri ->
          case (uriScheme uri, uriAuthority uri) of
            (scheme, Just auth)
              | scheme `notElem` ["http:", "https:"] -> return (Left "scheme must be http or https")
              | not (null (uriUserInfo auth)) -> return (Left "user info in URL")
              | otherwise -> do
                  let host = stripBrackets (uriRegName auth)
                      port = case uriPort auth of
                        (':' : digits) | not (null digits) && all isDigit digits -> read digits
                        _ -> if scheme == "https:" then 443 else 80
                  addrs <- resolve host
                  case addrs of
                    [] -> return (Left "host does not resolve")
                    _ | any (not . isPublic) addrs -> return (Left "host resolves to a non-public address")
                    (a : _) -> return (Right (host, port, showForResolve a))
            _ -> return (Left "no host")
  where
    stripBrackets h = if "[" `isPrefixOf` h then filter (`notElem` "[]") h else h

-- RFC 3986 characters only: no spaces, quotes, backslashes, braces or
-- control characters (the URL also ends up inside LaTeX error reports).
urlChar :: Char -> Bool
urlChar c = isAsciiLower c || isAsciiUpper c || isDigit c || c `elem` "-._~:/?#[]@!$&'()*+,;=%"

resolve :: String -> IO [SockAddr]
resolve host = do
  result <- try (getAddrInfo (Just defaultHints { addrSocketType = Stream }) (Just host) Nothing)
  case result of
    Left (_ :: SomeException) -> return []
    Right infos -> return (map addrAddress infos)

isPublic :: SockAddr -> Bool
isPublic (SockAddrInet _ h) = publicV4 (hostAddressToTuple h)
isPublic (SockAddrInet6 _ _ h _) =
  let (a, b, c, d, e, f, g, i) = hostAddress6ToTuple h
  in case (a, b, c, d, e, f) of
       (0, 0, 0, 0, 0, 0xffff) ->           -- IPv4-mapped: check the IPv4 part
         publicV4 (fromIntegral (g `shiftR` 8), fromIntegral (g .&. 0xff),
                   fromIntegral (i `shiftR` 8), fromIntegral (i .&. 0xff))
       _ -> not (a == 0 && b == 0 && c == 0 && d == 0 && e == 0 && f == 0 && g == 0 && i <= 1)  -- :: and ::1
            && (a .&. 0xfe00) /= 0xfc00      -- fc00::/7 unique local
            && (a .&. 0xffc0) /= 0xfe80      -- fe80::/10 link local
            && (a .&. 0xff00) /= 0xff00      -- ff00::/8 multicast
isPublic _ = False

publicV4 :: (Word8, Word8, Word8, Word8) -> Bool
publicV4 (a, b, _, _) =
  not (  a == 0 || a == 10 || a == 127              -- this network, private, loopback
      || (a == 169 && b == 254)                     -- link local, incl. cloud metadata
      || (a == 172 && b >= 16 && b <= 31)           -- private
      || (a == 192 && b == 168)                     -- private
      || (a == 100 && b >= 64 && b <= 127)          -- carrier-grade NAT
      || a >= 224 )                                 -- multicast and reserved

showForResolve :: SockAddr -> String
showForResolve (SockAddrInet _ h) =
  let (a, b, c, d) = hostAddressToTuple h
  in show a ++ "." ++ show b ++ "." ++ show c ++ "." ++ show d
showForResolve (SockAddrInet6 _ _ h _) =
  let (a, b, c, d, e, f, g, i) = hostAddress6ToTuple h
  in "[" ++ concatMap (\(n, x) -> (if n > 0 then ":" else "") ++ hex x) (zip [0 :: Int ..] [a, b, c, d, e, f, g, i]) ++ "]"
  where hex x = let s = showHexWord x in if null s then "0" else s
        showHexWord 0 = ""
        showHexWord x = showHexWord (x `div` 16) ++ ["0123456789abcdef" !! fromIntegral (x `mod` 16)]
showForResolve _ = ""
