module Parsers.MainTags where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text
import Data.Text.Encoding (decodeUtf8)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64

import System.IO.Unsafe (unsafePerformIO)

import System.FilePath (takeExtension)

import Ast.AbstractSyntaxTree
import Parsers.Paragraphs

parseMainContent :: Parser MainSection
parseMainContent =  parseCommentary <|> parseTable <|> parseChap <|> parseAbntChap <|> parseSummary <|> parseRef <|> parseList <|> parseLink <|> parseImage <|> parseVideo <|> parseAudio <|> parseCode <|> parseAbnt <|> parseContent


parseJustParagraph :: String -> Parser [MainSection]
parseJustParagraph st = manyTill parseContent (lookAhead (string st))

parseStrictDefault :: String -> Parser [MainSection]
parseStrictDefault st = manyTill parseDefault (lookAhead (string st))

parseTable :: Parser MainSection
parseTable = do
    _ <- string "(table)"
    _ <- many (char ' ' <|> char '\n')
    header <- parseTableHeader
    _ <- many (char ' ' <|> char '\n')
    rows <- manyTill parseTableRow (string "(/table)")
    return (Table header rows)

    where
    parseTableHeader :: Parser [String]
    parseTableHeader = do
        _ <- string "("
        header <- manyTill anySingle (string ")")
        _ <- many (char ' ' <|> char '\n')
        return $ fmap unpack (splitOn (pack " | ") (pack header))


    parseTableRow :: Parser [String]
    parseTableRow = do
        _ <- string "("
        row <- manyTill anySingle (string ")")
        _ <- many (char ' ' <|> char '\n')
        return $ fmap unpack (splitOn (pack " | ") (pack row))

parseRef :: Parser MainSection
parseRef = do
    _ <- string "(ref |"
    url    <- manyTill anySingle (string " | ")
    author <- manyTill anySingle (string " | ")
    title  <- manyTill anySingle (string " | ")
    year   <- manyTill anySingle (string " | ")
    access <- manyTill anySingle (string ")")
    content <- parseParagraphTill "(/ref)"
    _ <- string "(/ref)"
    return (Ref url author title year access content)

parseList :: Parser MainSection
parseList = do
    _     <- string "(>> |"
    title <- manyTill anySingle (string ")")
    content <- parseListBody "(/>>)"
    _     <- string "(/>>)"
    return (List title content)

    where
    parseListBody :: String -> Parser [MainSection]
    parseListBody stopMark =
        manyTill parseMainContent (lookAhead (string stopMark))

parseCommentary :: Parser MainSection
parseCommentary = do
    _ <- string "(-- "
    content <- manyTill anySingle (string " --)")
    return (Commentary content)


parseChap :: Parser MainSection
parseChap = do
    _     <- string "(chap |"
    mNum  <- optional $ try (do
               num <- manyTill anySingle (string " | ")
               return num)
    title <- manyTill anySingle (string ")")
    content <- parseListBody "(/chap)"
    _     <- string "(/chap)"
    return $ case mNum of
               Just number -> Abntchapter number title content
               Nothing     -> Chap title content

  where
    parseListBody :: String -> Parser [MainSection]
    parseListBody stopMark =
        manyTill parseMainContent (lookAhead (string stopMark))

parseLink :: Parser MainSection
parseLink = do
    _ <- string "(link | "
    url <- manyTill anySingle (string ")")
    content <- parseStrictDefault "(/link)"
    _ <- string "(/link)"
    return (Link url content)

convertToBase64 :: FilePath -> IO String
convertToBase64 path = do
    bytes <- BS.readFile path
    return $ unpack $ decodeUtf8 (B64.encode bytes)

parseImage :: Parser MainSection
parseImage = do
    _        <- string "(localimg | "
    resource <- manyTill anySingle (string ")")
    let extension = takeExtension resource
    content  <- parseStrictDefault "(/localimg)"
    _        <- string "(/localimg)"
    let b64res = unsafePerformIO (convertToBase64 resource)
    return (Image b64res extension content)

parseImageUrl :: Parser MainSection
parseImageUrl = do
    _        <- string "(img | "
    url      <- manyTill anySingle (string ")")
    content  <- parseStrictDefault "(/img)"
    _        <- string "(/img)"
    return (ImageUrl url content)

parseCode :: Parser MainSection
parseCode = do
    _ <- string "(code)"
    content <- parseStrictDefault "(/code)"
    _ <- string "(/code)"
    return (Code content)

parseVideo :: Parser MainSection
parseVideo = do
    _ <- string "(video | "
    url <- manyTill anySingle (string ")")
    content <- parseStrictDefault "(/video)"
    _ <- string "(/video)"
    return (Video url content)

parseAudio :: Parser MainSection
parseAudio = do
    _ <- string "(audio | "
    url <- manyTill anySingle (string ")")
    content <- parseStrictDefault "(/audio)"
    _ <- string "(/audio)"
    return (Audio url content)

parseQuote :: Parser MainSection
parseQuote = do
    _ <- string "(quote | "
    author <- manyTill anySingle (string ")")
    content <- parseJustParagraph "(/quote)"
    _ <- string "(/quote)"
    return (Quote author content)

parseAbntContent :: Parser MetaSection
parseAbntContent = parseAuthor <|> parseInstitution <|> parseSubtitle <|> parseLocation <|> parseYear <|> parseDescription

parseAbnt :: Parser MainSection
parseAbnt = do
    _ <- string "(abnt)"
    _ <- many (char ' ' <|> char '\n')
    content <- manyTill parseAbntContent (string "(/abnt)")
    _ <- many (char ' ' <|> char '\n')
    return (Meta content)

parseAuthor :: Parser MetaSection
parseAuthor = do
    _ <- string "(author)"
    _ <- many (char ' ' <|> char '\n')
    content <- manyTill anySingle (string "(/author)")
    _ <- many (char ' ' <|> char '\n')
    return (Author content)

parseInstitution :: Parser MetaSection
parseInstitution = do
    _ <- string "(institution)"
    _ <- many (char ' ' <|> char '\n')
    content <- manyTill anySingle (string "(/institution)")
    _ <- many (char ' ' <|> char '\n')
    return (Institution content)

parseSubtitle :: Parser MetaSection
parseSubtitle = do
    _ <- string "(subtitle)"
    _ <- many (char ' ' <|> char '\n')
    content <- manyTill anySingle (string "(/subtitle)")
    _ <- many (char ' ' <|> char '\n')
    return (Subtitle content)

parseLocation :: Parser MetaSection
parseLocation = do
    _ <- string "(location)"
    _ <- many (char ' ' <|> char '\n')
    content <- manyTill anySingle (string "(/location)")
    _ <- many (char ' ' <|> char '\n')
    return (Location content)

parseYear :: Parser MetaSection
parseYear = do
    _ <- string "(year)"
    _ <- many (char ' ' <|> char '\n')
    content <- manyTill anySingle (string "(/year)")
    _ <- many (char ' ' <|> char '\n')
    return (Year content)

parseDescription :: Parser MetaSection
parseDescription = do
    _ <- string "(description)"
    _ <- many (char ' ' <|> char '\n')
    content <- manyTill anySingle (string "(/description)")
    _ <- many (char ' ' <|> char '\n')
    return (Description content)

parseSummary :: Parser MainSection
parseSummary = do
    _ <- string "(summary | "
    title <- manyTill anySingle (string ")")
    return (Summary title)

parseAbntChap :: Parser MainSection
parseAbntChap = do
    _     <- string "(abntchap |"
    number <- manyTill anySingle (string " | ")
    title <- manyTill anySingle (string ")")
    content <- parseListBody "(/abntchap)"
    _     <- string "(/abntchap)"
    return (Abntchapter number title content)

    where
    parseListBody :: String -> Parser [MainSection]
    parseListBody stopMark =
        manyTill parseMainContent (lookAhead (string stopMark))