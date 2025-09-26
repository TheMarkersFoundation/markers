module Parsers.Paragraphs where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Control.Monad (void)
import Control.Applicative ((<|>))

import Ast.AbstractSyntaxTree
import Parsers.Types (Parser)
import Parsers.AllTags

-- Parse indentation, treating both tabs and 4 spaces as a tab character
parseIndentation :: Parser String
parseIndentation = concat <$> many (tabOrSpaces)
  where
    tabOrSpaces = (char '\t' >> return "\t") <|>
                 (try (count 4 (char ' ')) >> return "\t") <|>
                 (count 1 (char ' ') >> return " ") 

-- Main entry point for parsing text tags
textTag :: Parser TextTag
textTag = choice
    [ parseColor
    , parseSmall
    , parseSuperscript
    , parseBold
    , parseItalic
    , parseCrossed
    , parseUnderlined
    , parseInlineCode
    , parsePlain
    ]

-- Parse content until a specific delimiter, handling nested tags
textContentTill :: String -> Parser [TextTag]
textContentTill end = manyTill (textTagExcept end) (lookAhead (string end))

-- Parse any text tag except those that would conflict with the current context
textTagExcept :: String -> Parser TextTag
textTagExcept end = choice
    [ parseColor
    , parseSmall
    , parseSuperscript
    , parseBold
    , parseItalic
    , parseCrossed
    , parseUnderlined
    , parseInlineCode
    , parsePlainTill end
    ]

parseCrossed :: Parser TextTag
parseCrossed = do
    _ <- string "(c)"
    content <- textContentTill "(/c)"
    _ <- string "(/c)"
    return (Crossed content)

parseUnderlined :: Parser TextTag
parseUnderlined = do
    _ <- string "(u)"
    content <- textContentTill "(/u)"
    _ <- string "(/u)"
    return (Underlined content)
    
parseBold :: Parser TextTag
parseBold = do
    _ <- string "(b)"
    content <- textContentTill "(/b)"
    _ <- string "(/b)"
    return (Bold content)

parseItalic :: Parser TextTag
parseItalic = do
    _ <- string "(i)"
    content <- textContentTill "(/i)"
    _ <- string "(/i)"
    return (Italic content)

parseInlineCode :: Parser TextTag
parseInlineCode = do
    _ <- string "(k)"
    content <- manyTill anySingle (string "(/k)")
    return (CodeInline content)

parseSmall :: Parser TextTag
parseSmall = do
    _ <- string "(sm)"
    content <- textContentTill "(/sm)"
    _ <- string "(/sm)"
    return (Small content)

parseSuperscript :: Parser TextTag
parseSuperscript = do
    _ <- string "(tp)"
    content <- textContentTill "(/tp)"
    _ <- string "(/tp)"
    return (Top content)

parseColor :: Parser TextTag
parseColor = do
    _ <- string "(color |"
    color <- manyTill anySingle (string ")")
    content <- textContentTill "(/color)"
    _ <- string "(/color)"
    return (Color color content)

parsePlain :: Parser TextTag
parsePlain = do
    -- Handle tabs and regular content
    content <- some (
        (char '\t' *> return '\t') <|>
        (notFollowedBy (choice (map (try . string) allTags)) *> anySingle)
        )
    return (Plain content)

parsePlainTill :: String -> Parser TextTag
parsePlainTill delimiter = do
    content <- some (
        try ((try (count 4 (char ' ') >> return "\t") <|> (char '\t' >> return "\t"))) <|>
        (notFollowedBy (choice (map (try . string) (delimiter:allTags))) *> (fmap (:[]) anySingle))
        )
    return (Plain (concat content))

parseTextTill :: String -> Parser [TextTag]
parseTextTill delimiter = manyTill textTag (lookAhead (string delimiter))

paragraphEnd :: Parser ()
paragraphEnd = void $ lookAhead (choice (map string allTags))

parseParagraph :: Parser MainSection
parseParagraph = do
    -- Parse indentation, treating 4 spaces as tab
    whitespace <- parseIndentation
    
    initialContent <- if not (null whitespace)
                     then return [Plain whitespace]
                     else return []

    restContent <- someTill textTag (lookAhead (choice (map (try . void . string) allTags)) <|> eof)
    return (Paragraph $ initialContent ++ restContent)

parseParagraphTill :: String -> Parser MainSection
parseParagraphTill delimiter = do
    whitespace <- many (char ' ' <|> char '\t')
    initialContent <- if not (null whitespace)
                     then return [Plain whitespace]
                     else return []
    restContent <- manyTill textTag (lookAhead (string delimiter))
    return (Paragraph $ initialContent ++ restContent)
