module Markers where

import Text.Megaparsec
import Text.Megaparsec.Char

import Ast.AbstractSyntaxTree
import Parsers.MainTags
import Parsers.Paragraphs
import Converter.To

parseTitle :: Parser String
parseTitle = do
    _ <- string "(title)"
    manyTill anySingle (string "(/title)")

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
  file <- readFile "tcc.mks"
  parseTest parseMarkers file
  writeFile "tcc.html" (convertToAbnt file)