module Language.Markers.Parser.Document where

import Control.Applicative (optional)
import Data.Void (Void)
import Language.Markers.Ast.Content (Content(..))
import Language.Markers.Ast.Tree
import Language.Markers.Parser.Content (parseBodyContents)
import Language.Markers.Parser.Meta (parseMetadataBlock)
import Text.Megaparsec (Parsec, takeRest, takeWhileP)
import Text.Megaparsec.Char (newline)

type Parser = Parsec Void String

parseDocument :: Parser Markers
parseDocument = do
    titleLine <- takeWhileP (Just "title") notLineEnd
    _ <- optional newline
    metas <- parseMetadataBlock
    _ <- optional newline
    body <- takeRest
    let sections = buildSections body
    pure $ Document (Preference titleLine metas) sections
  where
    notLineEnd c = c /= '\n' && c /= '\r'

buildSections :: String -> [Section]
buildSections body =
    case parseBodyContents body of
        [] -> []
        contents -> [Section "Body" contents]