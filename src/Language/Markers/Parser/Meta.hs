module Language.Markers.Parser.Meta where

import Control.Monad (void)
import Language.Markers.Ast.Tree
import Language.Markers.Parser.Text (Parser, parseAtom, parseAtomList)
import Text.Megaparsec (choice, eof, many, try, (<|>))
import Text.Megaparsec.Char (char, eol, hspace, string)

parseMetadataBlock :: Parser [Meta]
parseMetadataBlock = many (try parseMetaLine)

parseMetaLine :: Parser Meta
parseMetaLine = choice
    [ try parseAuthors
    , try parseInstitution
    , try parseSubtitle
    , try parseLocation
    , try parseDescription
    , try parseMentor
    , try parseKeywords
    , parseDate
    ]

parseAuthors :: Parser Meta
parseAuthors = Author <$> labelled "By" parseAuthorList
  where
    parseAuthorList = parseAtomList

parseInstitution :: Parser Meta
parseInstitution = Institution <$> labelled "At" parseAtom

parseSubtitle :: Parser Meta
parseSubtitle = Subtitle <$> labelled "Subtitle" parseAtom

parseLocation :: Parser Meta
parseLocation = Location <$> labelled "Location" parseAtom

parseDescription :: Parser Meta
parseDescription = Description <$> labelled "Description" parseAtom

parseMentor :: Parser Meta
parseMentor = Mentor <$> labelled "Mentor" parseAtom

parseKeywords :: Parser Meta
parseKeywords = Keywords <$> labelled "Keywords" parseAtomList

parseDate :: Parser Meta
parseDate = Date <$> labelled "Date" parseAtom

labelled :: String -> Parser a -> Parser a
labelled label valueParser = do
    _ <- string label
    _ <- char ':'
    _ <- hspace
    value <- valueParser
    lineTerminator
    pure value

lineTerminator :: Parser ()
lineTerminator = void eol <|> void eof
