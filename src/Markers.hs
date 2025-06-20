module Markers where

import Text.Megaparsec
import Text.Megaparsec.Char

import Ast.AbstractSyntaxTree
import Parsers.MainTags
import Parsers.Paragraphs
import Converter.To
import Data.Char (isSpace)

import qualified Data.Text.IO as T
import qualified Data.Text as T

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
    content <- manyTill parseMainContent eof
    return (MarkersMain title content)

parseFileWith :: (Markers -> String) -> String -> String
parseFileWith renderer text = case parse parseMarkers "" text of
    Left err -> errorBundlePretty err
    Right res -> renderer res

convertToHtml :: String -> String
convertToHtml = parseFileWith toHtml

convertToRaw :: String -> String
convertToRaw = parseFileWith toRaw

convertToMarkdown :: String -> String
convertToMarkdown = parseFileWith toMarkdown

convertToAbnt :: String -> String
convertToAbnt = parseFileWith toAbnt

main :: IO ()
main = do
  setLocaleEncoding utf8

  mks   <- readFile "tcc.mks"
  let html = convertToAbnt mks
  writeFile "tcc.html" html