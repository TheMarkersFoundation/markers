module Main where

import Control.Concurrent (Chan, MVar, dupChan, forkIO, modifyMVar_, newChan, newMVar, readChan, readMVar, threadDelay, writeChan)
import Control.Exception (SomeException, evaluate, finally, try)
import Control.Monad (forever, unless, void, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as B8
import Data.Char (isSpace, toLower)
import Data.List (findIndex, intercalate, isPrefixOf, tails)
import Data.Time.Clock (UTCTime)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Language.Markers.Ast.Content (Content(..))
import Language.Markers.Ast.Tree (Markers(..), Meta(..), Preferences(..), Section(..))
import Language.Markers.Ast.Types (File(..))
import Language.Markers.Converter.Abnt (toAbnt)
import Language.Markers.Converter.Html (toHtml)
import Language.Markers.Parser.Document (parseDocument)
import Network.Socket
import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist, getModificationTime)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath (replaceExtension, takeDirectory, takeExtension, (</>), isRelative, normalise)
import System.IO (BufferMode (LineBuffering), Handle, IOMode (ReadWriteMode, WriteMode), hClose, hFlush, hGetLine, hIsEOF, hPutStr, hSetBuffering, hSetEncoding, stdout, withFile)
import Text.Megaparsec (errorBundlePretty, parse, eof)

data OutputFormat
  = HtmlFormat
  | AbntFormat
  deriving (Eq, Show)

data RunMode
  = WatchRun
  | ExportRun
  deriving (Eq, Show)

parseFile :: FilePath -> String -> Either String Markers
parseFile source text =
  case parse (parseDocument <* eof) source text of
    Left err -> Left (errorBundlePretty err)
    Right res -> Right res

convertTo :: OutputFormat -> FilePath -> String -> IO (Either String String)
convertTo outFormat source text =
  case parseFile source text of
    Left err -> pure (Left err)
    Right doc ->
      case validateForFormat outFormat doc of
        Left validationErr -> pure (Left validationErr)
        Right () -> do
          embeddedDoc <- embedImagesInDocument source doc
          let renderer = case outFormat of
                HtmlFormat -> toHtml
                AbntFormat -> toAbnt
          pure (Right (renderer embeddedDoc))

parseFormat :: String -> Maybe OutputFormat
parseFormat "--html" = Just HtmlFormat
parseFormat "--abnt" = Just AbntFormat
parseFormat "--format=html" = Just HtmlFormat
parseFormat "--format=abnt" = Just AbntFormat
parseFormat _ = Nothing

resolveArguments :: [String] -> Either String (FilePath, FilePath, OutputFormat, RunMode)
resolveArguments args = go args Nothing HtmlFormat WatchRun
  where
    go [] Nothing _ _ = Left usageMessage
    go [] (Just inp) fmt mode = Right (inp, defaultOutputFor inp, fmt, mode)
    go (arg:rest) maybeInput fmt mode
      | arg == "--watch" = go rest maybeInput fmt WatchRun
      | arg == "--export" = go rest maybeInput fmt ExportRun
      | Just parsedFmt <- parseFormat arg = go rest maybeInput parsedFmt mode
      | isOption arg = Left usageMessage
      | otherwise =
          case maybeInput of
            Nothing -> go rest (Just arg) fmt mode
            Just _ -> Left usageMessage

    isOption ('-':_) = True
    isOption _ = False

usageMessage :: String
usageMessage = "Usage: Markers <input-file> [--html|--abnt|--format=html|--format=abnt] [--watch] [--export]"

validateForFormat :: OutputFormat -> Markers -> Either String ()
validateForFormat HtmlFormat _ = Right ()
validateForFormat AbntFormat doc = validateAbntMetadata doc

validateAbntMetadata :: Markers -> Either String ()
validateAbntMetadata (Document (Preference _ metas) _) =
  let missing = concat
        [ require "By" hasAuthor
        , require "At" hasInstitution
        , require "Location" hasLocation
        , require "Date" hasDate
        , require "Description" hasDescription
        , require "Keywords" hasKeywords
        ]
  in case missing of
      [] -> Right ()
      xs -> Left ("ABNT requires metadata fields: " ++ intercalate ", " xs)
  where
    require :: String -> Bool -> [String]
    require _ True = []
    require field False = [field]

    hasAuthor =
      any (\m -> case m of
        Author names -> any hasContent names
        _ -> False) metas

    hasInstitution =
      any (\m -> case m of
        Institution value -> hasContent value
        _ -> False) metas

    hasLocation =
      any (\m -> case m of
        Location value -> hasContent value
        _ -> False) metas

    hasDate =
      any (\m -> case m of
        Date value -> hasContent value
        _ -> False) metas

    hasDescription =
      any (\m -> case m of
        Description value -> hasContent value
        _ -> False) metas

    hasKeywords =
      any (\m -> case m of
        Keywords values -> any hasContent values
        _ -> False) metas

    hasContent :: String -> Bool
    hasContent = not . null . trim

    trim :: String -> String
    trim = f . f
      where
        f = reverse . dropWhile isSpace

defaultOutputFor :: FilePath -> FilePath
defaultOutputFor path = replaceExtension path ".html"

readFileStrict :: FilePath -> IO String
readFileStrict filePath = do
  content <- readFile filePath
  _ <- evaluate (length content)
  pure content

embedImagesInDocument :: FilePath -> Markers -> IO Markers
embedImagesInDocument sourcePath (Document prefs sections) = do
  updatedSections <- mapM embedImagesInSection sections
  pure (Document prefs updatedSections)
  where
    baseDir = takeDirectory sourcePath

    embedImagesInSection :: Section -> IO Section
    embedImagesInSection (Section title contents) = do
      updatedContents <- mapM embedImagesInContent contents
      pure (Section title updatedContents)

    embedImagesInContent :: Content -> IO Content
    embedImagesInContent (Url (Image path) caption) = do
      embeddedPath <- pathToDataUri baseDir path
      pure (Url (Image embeddedPath) caption)
    embedImagesInContent (Figure (Image path) caption source) = do
      embeddedPath <- pathToDataUri baseDir path
      pure (Figure (Image embeddedPath) caption source)
    embedImagesInContent (Chapter level title contents) = do
      updatedContents <- mapM embedImagesInContent contents
      pure (Chapter level title updatedContents)
    embedImagesInContent (ArrowList level title contents) = do
      updatedContents <- mapM embedImagesInContent contents
      pure (ArrowList level title updatedContents)
    embedImagesInContent (BulletList level items) = do
      updatedItems <- mapM embedImagesInContent items
      pure (BulletList level updatedItems)
    embedImagesInContent content = pure content

pathToDataUri :: FilePath -> FilePath -> IO FilePath
pathToDataUri baseDir rawPath
  | "data:" `isPrefixOf` rawPath = pure rawPath
  | otherwise = do
      let resolvedPath =
            if isRelative rawPath
              then normalise (baseDir </> rawPath)
              else rawPath
      fileResult <- try (BS.readFile resolvedPath) :: IO (Either SomeException BS.ByteString)
      case fileResult of
        Left _ -> pure rawPath
        Right fileBytes -> do
          let mimeType = detectMimeType resolvedPath
              encoded = B8.unpack (B64.encode fileBytes)
          pure ("data:" ++ mimeType ++ ";base64," ++ encoded)

detectMimeType :: FilePath -> String
detectMimeType path =
  case map toLower (takeExtension path) of
    ".png" -> "image/png"
    ".jpg" -> "image/jpeg"
    ".jpeg" -> "image/jpeg"
    ".gif" -> "image/gif"
    ".bmp" -> "image/bmp"
    ".webp" -> "image/webp"
    ".svg" -> "image/svg+xml"
    _ -> "application/octet-stream"

renderToOutput :: OutputFormat -> FilePath -> FilePath -> String -> IO (Either String String)
renderToOutput outFormat inputPath outputPath content = do
  result <- convertTo outFormat inputPath content
  case result of
    Left err -> pure (Left err)
    Right html -> do
      withFile outputPath WriteMode $ \handle -> do
        hSetEncoding handle utf8
        hPutStr handle html
      when (outFormat == AbntFormat) (ensureAbntAssets outputPath)
      pure (Right html)

ensureAbntAssets :: FilePath -> IO ()
ensureAbntAssets outputPath = do
  let sourcePolyfill = normalise ("resources" </> "js" </> "polyfill.js")
      targetJsDir = takeDirectory outputPath </> "js"
      targetPolyfill = targetJsDir </> "polyfill.js"
  sourceExists <- doesFileExist sourcePolyfill
  when sourceExists $ do
    createDirectoryIfMissing True targetJsDir
    copyFile sourcePolyfill targetPolyfill

runSingle :: OutputFormat -> FilePath -> FilePath -> IO ()
runSingle outFormat inputPath outputPath = do
  content <- readFileStrict inputPath
  result <- renderToOutput outFormat inputPath outputPath content
  case result of
    Left err -> do
      putStrLn "Failed to parse document:\n"
      putStrLn err
      exitFailure
    Right _ ->
      putStrLn $ "Document parsed at " ++ outputPath ++ " (" ++ show outFormat ++ ")\n"

watchLoop :: OutputFormat -> FilePath -> FilePath -> IO ()
watchLoop outFormat inputPath outputPath = do
  putStrLn $ "Watching " ++ inputPath ++ " -> " ++ outputPath ++ " (" ++ show outFormat ++ ")"
  firstRead <- safeRead inputPath
  case firstRead of
    Left readErr -> do
      putStrLn $ "Could not read input file: " ++ readErr
      exitFailure
    Right firstContent -> do
      currentHtml <- newMVar (loadingDocument outputPath)
      updates <- newChan
      firstModTime <- safeModTime inputPath
      startLivePreviewServer currentHtml updates
      _ <- renderAndLog currentHtml updates firstContent
      loop currentHtml updates (either (const Nothing) Just firstModTime) firstContent
  where
    safeRead :: FilePath -> IO (Either String String)
    safeRead filePath = do
      result <- try (readFileStrict filePath) :: IO (Either SomeException String)
      pure $ case result of
        Left err -> Left (show err)
        Right content -> Right content

    safeModTime :: FilePath -> IO (Either String UTCTime)
    safeModTime filePath = do
      result <- try (getModificationTime filePath) :: IO (Either SomeException UTCTime)
      pure $ case result of
        Left err -> Left (show err)
        Right ts -> Right ts

    renderAndLog :: MVar String -> Chan () -> String -> IO Bool
    renderAndLog htmlVar updates content = do
      result <- renderToOutput outFormat inputPath outputPath content
      case result of
        Left err -> do
          putStrLn "Failed to parse document:\n"
          putStrLn err
          modifyMVar_ htmlVar (\_ -> pure (errorDocument err))
          writeChan updates ()
          pure False
        Right html -> do
          modifyMVar_ htmlVar (\_ -> pure html)
          writeChan updates ()
          putStrLn $ "Updated " ++ outputPath
          pure True

    startLivePreviewServer :: MVar String -> Chan () -> IO ()
    startLivePreviewServer htmlVar updates = do
      serverResult <- try (openServerSocket "127.0.0.1" livePreviewPort) :: IO (Either SomeException Socket)
      case serverResult of
        Left err ->
          putStrLn $ "Live preview disabled: " ++ show err
        Right serverSocket -> do
          putStrLn $ "Live preview running at http://127.0.0.1:" ++ show livePreviewPort ++ "/"
          void . forkIO $ acceptLoop serverSocket htmlVar updates

    openServerSocket :: String -> Int -> IO Socket
    openServerSocket host port = withSocketsDo $ do
      let hints = defaultHints { addrSocketType = Stream }
      address:_ <- getAddrInfo (Just hints) (Just host) (Just (show port))
      sock <- socket (addrFamily address) (addrSocketType address) (addrProtocol address)
      setSocketOption sock ReuseAddr 1
      bind sock (addrAddress address)
      listen sock 5
      pure sock

    acceptLoop :: Socket -> MVar String -> Chan () -> IO ()
    acceptLoop serverSocket htmlVar updates =
      forever $ do
        (connection, _) <- accept serverSocket
        void . forkIO $ do
          _ <- try (serveClient connection htmlVar updates) :: IO (Either SomeException ())
          pure ()

    serveClient :: Socket -> MVar String -> Chan () -> IO ()
    serveClient connection htmlVar updates = do
      handle <- socketToHandle connection ReadWriteMode
      hSetEncoding handle utf8
      hSetBuffering handle LineBuffering
      finally
        (do
          requestLine <- hGetLine handle
          consumeHeaders handle
          case requestPath requestLine of
            "/events" -> serveEvents handle updates
            "/js/polyfill.js" -> do
              polyfillResult <- try (readFileStrict (normalise ("resources" </> "js" </> "polyfill.js"))) :: IO (Either SomeException String)
              case polyfillResult of
                Left _ ->
                  sendResponse handle "404 Not Found" "text/plain; charset=utf-8" "polyfill.js not found"
                Right jsBody ->
                  sendResponse handle "200 OK" "application/javascript; charset=utf-8" jsBody
            "/document" -> do
              html <- readMVar htmlVar
              sendResponse handle "200 OK" "text/html; charset=utf-8" (injectLiveReloadScript html)
            "/" -> do
              html <- readMVar htmlVar
              sendResponse handle "200 OK" "text/html; charset=utf-8" (injectLiveReloadScript html)
            "/index.html" -> do
              html <- readMVar htmlVar
              sendResponse handle "200 OK" "text/html; charset=utf-8" (injectLiveReloadScript html)
            _ ->
              sendResponse handle "404 Not Found" "text/plain; charset=utf-8" "Not Found")
        (hClose handle)

    requestPath :: String -> String
    requestPath requestLine =
      case words (stripCR requestLine) of
        (_method:path:_) -> path
        _ -> "/"

    consumeHeaders :: Handle -> IO ()
    consumeHeaders handle = do
      endOfFile <- hIsEOF handle
      unless endOfFile $ do
        line <- hGetLine handle
        if null (stripCR line)
          then pure ()
          else consumeHeaders handle

    sendResponse :: Handle -> String -> String -> String -> IO ()
    sendResponse handle status contentType body = do
      hPutStr handle ("HTTP/1.1 " ++ status ++ "\r\n")
      hPutStr handle ("Content-Type: " ++ contentType ++ "\r\n")
      hPutStr handle "Cache-Control: no-cache\r\n"
      hPutStr handle "Connection: close\r\n\r\n"
      hPutStr handle body
      hFlush handle

    serveEvents :: Handle -> Chan () -> IO ()
    serveEvents handle updates = do
      clientUpdates <- dupChan updates
      do
        hPutStr handle "HTTP/1.1 200 OK\r\n"
        hPutStr handle "Content-Type: text/event-stream\r\n"
        hPutStr handle "Cache-Control: no-cache\r\n"
        hPutStr handle "Connection: keep-alive\r\n\r\n"
        hPutStr handle "event: ready\ndata: connected\n\n"
        hFlush handle
        forever $ do
          _ <- readChan clientUpdates
          hPutStr handle "event: update\ndata: changed\n\n"
          hFlush handle

    stripCR :: String -> String
    stripCR s =
      case reverse s of
        ('\r':rest) -> reverse rest
        _ -> s

    loadingDocument :: FilePath -> String
    loadingDocument path =
      "<!DOCTYPE html><html><head><meta charset=\"UTF-8\"><title>Markers</title></head><body><p>Watching " ++ escapeBasic path ++ "...</p></body></html>"

    errorDocument :: String -> String
    errorDocument err =
      "<!DOCTYPE html><html><head><meta charset=\"UTF-8\"><title>Parse Error</title></head><body><pre>" ++ escapeBasic err ++ "</pre></body></html>"

    escapeBasic :: String -> String
    escapeBasic = concatMap esc
      where
        esc '&' = "&amp;"
        esc '<' = "&lt;"
        esc '>' = "&gt;"
        esc '"' = "&quot;"
        esc '\'' = "&#39;"
        esc c = [c]

    liveReloadScript :: String
    liveReloadScript =
      "<script>(function(){let reloading=false;if(window.__markersEs){try{window.__markersEs.close();}catch(_e){}}const es=new EventSource('/events');window.__markersEs=es;es.addEventListener('update',async function(){if(reloading)return;reloading=true;try{const r=await fetch('/document',{cache:'no-store'});if(!r.ok){reloading=false;return;}const html=await r.text();try{es.close();}catch(_e){}document.open();document.write(html);document.close();}catch(_e){reloading=false;}});window.addEventListener('beforeunload',function(){try{es.close();}catch(_e){}});})();</script>"

    injectLiveReloadScript :: String -> String
    injectLiveReloadScript html =
      case findIndex (isPrefixOf "</body>") (tails html) of
        Just idx ->
          let (prefix, suffix) = splitAt idx html
          in prefix ++ liveReloadScript ++ suffix
        Nothing -> html ++ liveReloadScript

    livePreviewPort :: Int
    livePreviewPort = 4173

    loop :: MVar String -> Chan () -> Maybe UTCTime -> String -> IO ()
    loop htmlVar updates previousModTime previousContent = do
      threadDelay 1000000
      currentMod <- safeModTime inputPath
      case currentMod of
        Left _ ->
          loop htmlVar updates previousModTime previousContent
        Right currentModTime ->
          if Just currentModTime == previousModTime
            then loop htmlVar updates previousModTime previousContent
            else do
              currentRead <- safeRead inputPath
              case currentRead of
                Left _ ->
                  loop htmlVar updates previousModTime previousContent
                Right currentContent ->
                  if currentContent == previousContent
                    then loop htmlVar updates (Just currentModTime) previousContent
                    else do
                      _ <- renderAndLog htmlVar updates currentContent
                      loop htmlVar updates (Just currentModTime) currentContent

main :: IO ()
main = do
  setLocaleEncoding utf8
  hSetBuffering stdout LineBuffering
  args <- getArgs
  case resolveArguments args of
    Left err -> do
      putStrLn err
      exitFailure
    Right (inputPath, outputPath, outFormat, runMode) ->
      case runMode of
        WatchRun -> watchLoop outFormat inputPath outputPath
        ExportRun -> runSingle outFormat inputPath outputPath
