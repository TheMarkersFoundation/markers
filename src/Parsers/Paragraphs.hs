module Parsers.Paragraphs where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Control.Monad (void)
import Ast.AbstractSyntaxTree

type Parser = Parsec Void String

parseDefaultTagless :: Parser MainSection
parseDefaultTagless = do
  content <- someTill anySingle (void (lookAhead tagStart) <|> eof)
  return (Paragraph (Default content))
  where
    tagStart = char '(' <|> char '\n'

parseDefault :: Parser MainSection
parseDefault = do
  content <- someTill anySingle (void (lookAhead tagStart) <|> eof)
  return (Paragraph (Default content))
  where
    tagStart :: Parser String
    tagStart = choice
      [ try (string "(c)")
      , try (string "(u)")
      , try (string "(b)")
      , try (string "(i)")
      , try (string "(n)")
      , try (string "(k)")
      , try (string "(p)")
      , try (string "(sm)")
      , try (string "(tp)")
      , try (string "(color |")
      , try (string "(hr)")
      , try (string "(tab)")
      , try (string "(table)")
      , try (string "(ref |")
      , try (string "(>> |")
      , try (string "(-- ")
      , try (string "(chap |")
      , try (string "(link | ")
      , try (string "(localimg | ")
      , try (string "(img | ")
      , try (string "(code)")
      , try (string "(video | ")
      , try (string "(audio | ")
      , try (string "(quote | ")
      , try (string "(meta)")
      , try (string "(summary |")
      , try (string "(/c)")
      , try (string "(/u)")
      , try (string "(/b)")
      , try (string "(/i)")
      , try (string "(/n)")
      , try (string "(/k)")
      , try (string "(/p)")
      , try (string "(/sm)")
      , try (string "(/tp)")
      , try (string "(/color)")
      , try (string "(/hr)")
      , try (string "(/tab)")
      , try (string "(/table)")
      , try (string "(/ref)")
      , try (string "(/>>)")
      , try (string "--) ")
      , try (string "(/chap)")
      , try (string "(/link)")
      , try (string "(/localimg)")
      , try (string "(/img)")
      , try (string "(/code)")
      , try (string "(/video)")
      , try (string "(/audio)")
      , try (string "(/quote)")
      , try (string "(/meta)")
      , try (string "(/summary)")
      , try (string "(trace |")
      , try (string "(/trace)")
      , try (string "\n")
      ]

parseCrossed :: Parser MainSection
parseCrossed = do
    _ <- string "(c)"
    content <- manyTill anySingle (string "(/c)")
    return (Paragraph (Crossed content))

parseUnderlined :: Parser MainSection
parseUnderlined = do
    _ <- string "(u)"
    content <- manyTill anySingle (string "(/u)")
    return (Paragraph (Underlined content))
    
parseBold :: Parser MainSection
parseBold = do
    _ <- string "(b)"
    content <- manyTill anySingle (string "(/b)")
    return (Paragraph (Bold content))

parseItalic :: Parser MainSection
parseItalic = do
    _ <- string "(i)"
    content <- manyTill anySingle (string "(/i)")
    return (Paragraph (Italic content))

parseBoldItalic :: Parser MainSection
parseBoldItalic = do
    _ <- string "(n)"
    content <- manyTill anySingle (string "(/n)")
    return (Paragraph (BoldItalic content))

parseInlineCode :: Parser MainSection
parseInlineCode = do
    _ <- string "(k)"
    content <- manyTill anySingle (string "(/k)")
    return (Paragraph (CodeInline content))

parseForceDefault :: Parser MainSection
parseForceDefault = do
    _ <- string "(p)"
    content <- manyTill anySingle (string "(/p)")
    return (Paragraph (Default content))

parseSmall :: Parser MainSection
parseSmall = do
    _ <- string "(sm)"
    content <- manyTill anySingle (string "(/sm)")
    return (Paragraph (Small content))

parseSuperscript :: Parser MainSection
parseSuperscript = do
    _ <- string "(tp)"
    content <- manyTill anySingle (string "(/tp)")
    return (Paragraph (Top content))

parseColor :: Parser MainSection
parseColor = do
    _ <- string "(color |"
    color <- manyTill anySingle (string ")")
    content <- manyTill anySingle (string "(/color)")
    return (Paragraph (Color color content))

parseLineBreak :: Parser MainSection
parseLineBreak = do
    _ <- newline
    return LineBreak

parseSeparator :: Parser MainSection
parseSeparator = do
    _ <- string "(hr)"
    return Separator

parseTab :: Parser MainSection
parseTab = do
    _ <- string "(tab)"
    return Tab

parseParagraph :: Parser [MainSection]
parseParagraph = many parseContent

parseContent :: Parser MainSection
parseContent =  parseSeparator <|> parseColor <|> parseLineBreak <|> parseSmall <|> parseSuperscript <|> parseBoldItalic <|> parseBold <|> parseItalic <|> parseCrossed <|> parseUnderlined <|> parseInlineCode <|> parseForceDefault <|> parseDefault

parseParagraphTill :: String -> Parser [MainSection]
parseParagraphTill st = manyTill parseContent (lookAhead (string st))

