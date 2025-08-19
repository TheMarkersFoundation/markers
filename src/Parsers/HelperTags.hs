module Parsers.HelperTags where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Control.Monad (void)

import Ast.AbstractSyntaxTree

import Parsers.Types (Parser)

parseLineBreak :: Parser Helpies
parseLineBreak = do
    _ <- newline
    return LineBreak

parseSeparator :: Parser Helpies
parseSeparator = do
    _ <- string "(hr)"
    return Separator

parseHelperTags :: Parser MainSection
parseHelperTags = do
    h <- parseLineBreak <|> parseSeparator
    return (Helpy h)