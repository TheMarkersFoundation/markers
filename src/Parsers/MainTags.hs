module Parsers.MainTags where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text
import Data.Text.Encoding (decodeUtf8)
import Control.Monad (when, void)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Text.Megaparsec.Char.Lexer as L

import System.IO.Unsafe (unsafePerformIO)

import System.FilePath (takeExtension)

import Ast.AbstractSyntaxTree
import Parsers.Paragraphs

parseMainContent :: Parser MainSection
parseMainContent =  parseCommentary <|> parseNumberedList <|> parseCentered <|> parseRightContent <|> parseReferences <|> parseFigureList <|> parseTable <|> parseQuote <|> parseChap <|> parseSummary <|> parseRef <|> parseList <|> parseLink <|> parseTrace <|> parseImageUrl <|> parseImage <|> parseVideo <|> parseAudio <|> parseCode <|> parseMeta <|> parseContent

parseJustParagraph :: String -> Parser [MainSection]
parseJustParagraph st = manyTill parseContent (lookAhead (string st))

parseStrictDefault :: String -> Parser [MainSection]
parseStrictDefault st = manyTill parseDefaultTagless (lookAhead (string st))

parseCentered :: Parser MainSection
parseCentered = do
    _ <- string "(align-center)"
    _ <- many (char ' ' <|> char '\n')
    content <- manyTill parseMainContent (string "(/align-center)")
    return (Centered content)

parseRightContent :: Parser MainSection
parseRightContent = do
    _ <- string "(align-right)"
    _ <- many (char ' ' <|> char '\n')
    content <- manyTill parseMainContent (string "(/align-right)")
    return (RightContent content)

parseTable :: Parser MainSection
parseTable = do
    _ <- string "(table)"
    _ <- many (char ' ' <|> char '\n')
    header <- parseTableContent
    _ <- many (char ' ' <|> char '\n')
    rows <- manyTill parseTableContent (string "(/table)")
    return (Table header rows)

    where
    parseTableContent :: Parser [String]
    parseTableContent = do
        _ <- string "("
        header <- manyTill anySingle (string ")")
        _ <- many (char ' ' <|> char '\n')
        return $ fmap unpack (splitOn (pack " | ") (pack header))

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

parseNumberedList :: Parser MainSection
parseNumberedList = do
  void (string "(list)")
  void eol
  items <- manyTill parseListItem (void $ string "(/list)")
  return $ NumberedList items
  where
    parseListItem :: Parser [MainSection]
    parseListItem = do
      skipMany (char ' ' <|> char '\t')
      void (L.decimal >> char '.' >> space1)
      tags <- manyTill parseContent $
           lookAhead (void eol)
        <|> lookAhead (void $ L.decimal >> char '.' >> space1)
        <|> lookAhead (void $ string "(/list)")
      void eol <|> void eof
      return tags

parseCommentary :: Parser MainSection
parseCommentary = do
    _ <- string "(-- "
    content <- manyTill anySingle (string " --)")
    return (Commentary content)


parseChap :: Parser MainSection
parseChap = do
  _     <- string "(chap |"
  _     <- many (char ' ')
  mNum  <- optional $ try $ do
    dígitos <- some digitChar
    -- salto o espaço, a barra e outro espaço
    _       <- char ' ' *> char '|' *> char ' '
    return dígitos
    
  title <- do 
    t <- takeWhileP (Just "chapter title") (/= ')')
    when (Prelude.length t > 54) $
      fail $ "chap tag title is longer than 54 characters: " ++ show t
    return t

  _     <- char ')'
  content <- parseChapBody "(/chap)"
  _       <- string "(/chap)"
  return $ case mNum of
    Just number -> Abntchapter number title content
    Nothing     -> Chap         title content
  where
    parseChapBody stopMark =
      manyTill parseMainContent (lookAhead (string stopMark))


parseLink :: Parser MainSection
parseLink = do
    _ <- string "(link | "
    url <- manyTill anySingle (string ")")
    content <- parseStrictDefault "(/link)"
    _ <- string "(/link)"
    return (Link url content)

parseTrace :: Parser MainSection
parseTrace = do
    _ <- string "(trace | "
    url <- manyTill anySingle (string ")")
    content <- parseStrictDefault "(/trace)"
    _ <- string "(/trace)"
    return (Trace url content)

convertToBase64 :: FilePath -> IO String
convertToBase64 path = do
  bytes <- BS.readFile path
  return $ unpack $ decodeUtf8 (B64.encode bytes)

parseImage :: Parser MainSection
parseImage = do
  _ <- string "(localimg |"
  space
  mNum <- optional $ try $ do
    num <- some digitChar
    space
    _ <- char '|'
    space
    return num
  resource <- manyTill anySingle (char ')')
  content <- do
    c <- manyTill anySingle (lookAhead (string "(/localimg)"))
    when (Prelude.length c > 54) $
      fail $ "localimg tag content is longer than 54 characters: " ++ c
    _ <- string "(/localimg)"
    return c
  let extension = takeExtension resource
      b64res    = unsafePerformIO (convertToBase64 resource)
      contentSections = [Paragraph (Default content)]
  return $ case mNum of
    Just number -> ImagePage number b64res extension contentSections
    Nothing     -> Image b64res extension contentSections

parseImageUrl :: Parser MainSection
parseImageUrl = do
  _ <- string "(img"
  space *> char '|' *> space
  mNum <- optional $ try $ do
    digits <- some digitChar
    space *> char '|' *> space
    return digits
  resource <- manyTill anySingle (char ')')
  content <- do
    c <- manyTill anySingle (lookAhead (string "(/img)"))
    when (Prelude.length c > 54) $
      fail $ "img tag content is longer than 54 characters: " ++ c
    _ <- string "(/img)"
    return c
  let contentSections = [Paragraph (Default content)]
  return $ case mNum of
    Just number -> ImageUrlPage number resource contentSections
    Nothing     -> ImageUrl resource contentSections

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

parseMetaContent :: Parser MetaSection
parseMetaContent = parseAuthor <|> parseInstitution <|> parseSubtitle <|> parseLocation <|> parseYear <|> parseDescription

parseMeta :: Parser MainSection
parseMeta = do
    _ <- string "(meta)"
    _ <- many (char ' ' <|> char '\n')
    content <- manyTill parseMetaContent (string "(/meta)")
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

parseReferences :: Parser MainSection
parseReferences = do
    _ <- string "(references)"
    return References

parseFigureList :: Parser MainSection
parseFigureList = do
    _ <- string "(figurelist)"
    return Figurelist