module Language.Markers.Parser.Text where

import Control.Applicative (many, some)
import Data.Char (isSpace)
import Data.Void (Void)
import Language.Markers.Ast.Text
import Language.Markers.Ast.Types (Atom, Color)
import Numeric (readHex)
import Text.Megaparsec (Parsec, count, manyTill, sepBy1, takeWhile1P, takeWhileP, try, (<|>))
import Text.Megaparsec.Char (char, digitChar, hspace, hexDigitChar, string)

type Parser = Parsec Void String

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

parseAtom :: Parser Atom
parseAtom = trim <$> takeWhile1P (Just "atom") notLineEnd
    where
        notLineEnd c = c /= '\n' && c /= '\r'

parseAtomList :: Parser [Atom]
parseAtomList = sepBy1 atomItem separator
  where
    separator = char ',' >> hspace
    atomItem = trim <$> takeWhile1P (Just "atom item") notTerminator
    notTerminator c = c /= ',' && c /= '\n' && c /= '\r'

parseBold :: Parser Writing
parseBold = parseBoldWith []

parseBoldWith :: [Char] -> Parser Writing
parseBoldWith extraStops = do
    _ <- char '*'
    contents <- many (parseWritingWith extraStops)
    _ <- char '*'
    return $ Bold contents

parseItalic :: Parser Writing
parseItalic = parseItalicWith []

parseItalicWith :: [Char] -> Parser Writing
parseItalicWith extraStops = do
    _ <- char '_'
    contents <- many (parseWritingWith extraStops)
    _ <- char '_'
    return $ Italic contents

parseUnderline :: Parser Writing
parseUnderline = parseUnderlineWith []

parseUnderlineWith :: [Char] -> Parser Writing
parseUnderlineWith extraStops = do
    _ <- string "__"
    contents <- many (parseWritingWith extraStops)
    _ <- string "__"
    return $ Underline contents

parseStrikethrough :: Parser Writing
parseStrikethrough = parseStrikethroughWith []

parseStrikethroughWith :: [Char] -> Parser Writing
parseStrikethroughWith extraStops = do
    _ <- char '~'
    contents <- many (parseWritingWith extraStops)
    _ <- char '~'
    return $ Strikethrough contents

parseMonospaced :: Parser Writing
parseMonospaced = parseMonospacedWith []

parseMonospacedWith :: [Char] -> Parser Writing
parseMonospacedWith extraStops = do
    _ <- char '`'
    contents <- many (parseWritingWith extraStops)
    _ <- char '`'
    return $ Monospaced contents

parseLinkWith :: [Char] -> Parser Writing
parseLinkWith extraStops = do
    _ <- char '['
    label <- manyTill (parseWritingWith (']' : extraStops)) (char ']')
    _ <- char '('
    url <- takeWhile1P (Just "link url") (/= ')')
    _ <- char ')'
    return $ Link label (trim url)

-- Reference syntax:
-- $[label](url|author|title|year|access)
parseReferenceWith :: [Char] -> Parser Writing
parseReferenceWith extraStops = do
    _ <- char '$'
    _ <- char '['
    label <- manyTill (parseWritingWith (']' : extraStops)) (char ']')
    _ <- char '('
    rawMeta <- takeWhileP (Just "reference metadata") (/= ')')
    _ <- char ')'
    case map trim (splitByPipe rawMeta) of
      [url, author, title, year, access] ->
          return $ Reference label url author title year access
      _ ->
          fail "invalid reference metadata, expected 5 fields"

parsePlain :: Parser Writing
parsePlain = parsePlainWith []

parsePlainWith :: [Char] -> Parser Writing
parsePlainWith extraStops = Plain <$> takeWhile1P (Just "plain text") plainChar
  where
    plainChar c = c `notElem` (specialChars ++ extraStops)
    specialChars = "[{*_~`$\n" -- These symbols should end parsing plain text.

parseDollar :: Parser Writing
parseDollar = do
    _ <- char '$'
    return (Plain "$")

parseColored :: Parser Writing
parseColored = do
    _ <- char '{'
    color <- try parseHexColor <|> parseRgbColor
    _ <- char '}'
    _ <- char '('
    contents <- manyTill (parseWritingWith [')']) (char ')')
    return $ Colored contents color

parseHexColor :: Parser Color
parseHexColor = do
    _ <- char '#'
    digits <- count 6 hexDigitChar
    let (rHex, rest1) = splitAt 2 digits
        (gHex, bHex) = splitAt 2 rest1
    return (hexPairToInt rHex, hexPairToInt gHex, hexPairToInt bHex)

hexPairToInt :: String -> Int
hexPairToInt hx = case readHex hx of
    [(value, "")] -> value
    _              -> 0

parseRgbColor :: Parser Color
parseRgbColor = do
    r <- rgbComponent
    _ <- char ','
    hspace
    g <- rgbComponent
    _ <- char ','
    hspace
    b <- rgbComponent
    return (r, g, b)

rgbComponent :: Parser Int
rgbComponent = do
    digits <- some digitChar
    let value = read digits
    if value >= 0 && value <= 255
        then return value
        else fail "RGB component out of range"

parseWriting :: Parser Writing
parseWriting = parseWritingWith []

parseWritingWith :: [Char] -> Parser Writing
parseWritingWith extraStops =
           try (parseBoldWith extraStops)
       <|> try (parseItalicWith extraStops)
       <|> try (parseUnderlineWith extraStops)
       <|> try (parseStrikethroughWith extraStops)
       <|> try (parseMonospacedWith extraStops)
       <|> try (parseReferenceWith extraStops)
       <|> try (parseLinkWith extraStops)
       <|> try parseColored
       <|> try parseDollar
       <|> parsePlainWith extraStops

parseWritings :: Parser [Writing]
parseWritings = many parseWriting

splitByPipe :: String -> [String]
splitByPipe [] = [""]
splitByPipe (c:cs)
  | c == '|' = "" : splitByPipe cs
  | otherwise =
      case splitByPipe cs of
        [] -> [[c]]
        (x:xs) -> (c:x) : xs
