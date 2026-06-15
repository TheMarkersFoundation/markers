module Language.Markers.Parser.Text where

import Control.Applicative (many, some)
import Data.Char (isSpace)
import Data.Void (Void)
import Language.Markers.Ast.Text
import Language.Markers.Ast.Types (Atom, Color)
import Numeric (readHex)
import Text.Megaparsec (Parsec, count, manyTill, sepBy1, takeWhile1P, try, (<|>))
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

-- Reference syntax (per-type sigil between '$' and '['):
-- $[label](url|author|title|year|access)                                  web (documento online)
-- $b[label](author|title|edition|city|publisher|year)                     book (livro)
-- $a[label](author|title|journal|volume|number|pages|year)                article (artigo de periodico)
-- $c[label](author|chapterTitle|bookAuthor|bookTitle|city|publisher|year|pages)  book chapter (capitulo)
-- $t[label](author|title|workType|institution|city|year)                  academic work (tese/dissertacao)
parseReferenceWith :: [Char] -> Parser Writing
parseReferenceWith extraStops = do
    _ <- char '$'
    refType <- parseRefSigil
    _ <- char '['
    label <- manyTill (parseWritingWith (']' : extraStops)) (char ']')
    _ <- char '('
    rawMeta <- parseReferenceMetadata
    _ <- char ')'
    let fields = map trim (splitByPipe rawMeta)
    if length fields == refArity refType
        then return $ Reference label refType fields
        else fail ("invalid reference metadata, expected "
                    ++ show (refArity refType) ++ " fields for "
                    ++ refLabel refType)

-- Reads the metadata between the reference's outer parentheses, allowing
-- balanced nested parens. ABNT work types such as "Dissertacao (Mestrado em
-- Educacao)" embed parentheses, so the terminating ')' is the unmatched one.
parseReferenceMetadata :: Parser String
parseReferenceMetadata = concat <$> many metaChunk
  where
    metaChunk = nested <|> plain
    plain = takeWhile1P (Just "reference metadata") (\c -> c /= '(' && c /= ')')
    nested = do
        _ <- char '('
        inner <- parseReferenceMetadata
        _ <- char ')'
        return ('(' : inner ++ ")")

-- The optional type letter that follows '$'. A bare '$[' is a web reference, so
-- when no known letter precedes the bracket we default to WebRef.
parseRefSigil :: Parser RefType
parseRefSigil =
        (char 'b' >> pure BookRef)
    <|> (char 'a' >> pure ArticleRef)
    <|> (char 'c' >> pure ChapterRef)
    <|> (char 't' >> pure ThesisRef)
    <|> pure WebRef

-- Number of pipe-separated fields each reference style requires. Optional
-- fields (edition, volume, number, pages) must still be present as empty slots.
refArity :: RefType -> Int
refArity WebRef = 5
refArity BookRef = 6
refArity ArticleRef = 7
refArity ChapterRef = 8
refArity ThesisRef = 6

refLabel :: RefType -> String
refLabel WebRef = "web reference"
refLabel BookRef = "book reference"
refLabel ArticleRef = "article reference"
refLabel ChapterRef = "chapter reference"
refLabel ThesisRef = "academic work reference"

-- Footnote syntax: ^[note text]
-- Produces an ABNT footnote: an inline, auto-numbered marker whose text is
-- rendered at the bottom of the page where the marker lands.
parseFootnoteWith :: [Char] -> Parser Writing
parseFootnoteWith extraStops = do
    _ <- char '^'
    _ <- char '['
    contents <- manyTill (parseWritingWith (']' : extraStops)) (char ']')
    return $ Footnote contents

-- Inline math: !math( ... ). Mirrors the block tag, but flows within a
-- paragraph. The body may contain balanced parentheses, so the closing ')' is
-- the unmatched one. (A line that *starts* with !math( is a display block and
-- is handled by the body parser, not here.)
parseMathInlineWith :: [Char] -> Parser Writing
parseMathInlineWith _ = do
    _ <- string "!math("
    body <- parseMathInlineBody
    _ <- char ')'
    return (MathInline (trim body))

parseMathInlineBody :: Parser String
parseMathInlineBody = concat <$> many metaChunk
  where
    metaChunk = nested <|> plain
    plain = takeWhile1P (Just "inline math") (\c -> c /= '(' && c /= ')')
    nested = do
        _ <- char '('
        inner <- parseMathInlineBody
        _ <- char ')'
        return ('(' : inner ++ ")")

parsePlain :: Parser Writing
parsePlain = parsePlainWith []

parsePlainWith :: [Char] -> Parser Writing
parsePlainWith extraStops = Plain <$> takeWhile1P (Just "plain text") plainChar
  where
    plainChar c = c `notElem` (specialChars ++ extraStops)
    specialChars = "[{*_~`$^!\n" -- These symbols should end parsing plain text.

parseDollar :: Parser Writing
parseDollar = do
    _ <- char '$'
    return (Plain "$")

-- Fallback for a lone '^' that does not open a footnote, mirroring parseDollar.
parseCaret :: Parser Writing
parseCaret = do
    _ <- char '^'
    return (Plain "^")

-- Fallback for a lone '!' that does not open an inline math span.
parseBang :: Parser Writing
parseBang = do
    _ <- char '!'
    return (Plain "!")

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
       <|> try (parseFootnoteWith extraStops)
       <|> try (parseMathInlineWith extraStops)
       <|> try (parseLinkWith extraStops)
       <|> try parseColored
       <|> try parseDollar
       <|> parseCaret
       <|> parseBang
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
