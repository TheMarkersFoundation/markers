module Markers where

import Text.Megaparsec
import Text.Megaparsec.Char

import Ast.AbstractSyntaxTree
import Parsers.MainTags
import Parsers.Paragraphs
import Parsers.PreferenceTags
import Converters.ToAbnt
import Converters.ToHtml
import Converters.ToLegacy

import Data.Char (isSpace)

import qualified Data.Text.IO as T
import qualified Data.Text as T

import Parsers.Types (Parser)

import System.IO (withFile, IOMode(WriteMode), hSetEncoding, utf8, hPutStr)
import GHC.IO.Encoding (setLocaleEncoding, utf8)

spaceConsumer :: Parser ()
spaceConsumer = skipMany (satisfy (\c -> isSpace c || c == '\xFEFF'))

parseTitle :: Parser String
parseTitle = do
  spaceConsumer
  _   <- string "(title)"
  txt <- manyTill anySingle (try (string "(/title)"))
  return txt

parseMarkers :: Parser Markers
parseMarkers = do
    title <- parseTitle
    space'
    preferences <- many (try parsePreferenceTag)
    content <- manyTill parseMainContent eof
    return (MarkersMain title (concat preferences) content)

parseFileWith :: (Markers -> String) -> String -> String
parseFileWith renderer text = case parse parseMarkers "" text of
    Left err -> errorBundlePretty err
    Right res -> renderer res

convertToAbnt :: String -> String
convertToAbnt = parseFileWith toAbnt

convertToHtml :: String -> String
convertToHtml = parseFileWith toHtml

convertToStyledHtml :: String -> String
convertToStyledHtml = parseFileWith toStyledHtml

main :: IO ()
main = do
  setLocaleEncoding utf8
  mks   <- readFile "readme.mks"
  let html = convertToHtml mks
  writeFile "readme.html" html