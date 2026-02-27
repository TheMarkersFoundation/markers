module Language.Markers.Parser.Content (parseBodyContents) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Char (isDigit, isSpace)
import Data.List (isPrefixOf, tails, findIndex)
import Data.Void (Void)
import Language.Markers.Ast.Content
import Language.Markers.Ast.Text (Writing(..))
import Language.Markers.Ast.Types (File(..))
import Language.Markers.Parser.Text (parseWritings)
import Text.Megaparsec
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char

type BodyParser = Parsec Void String

-- TODO: Clean this up later asap.

parseBodyContents :: String -> [Content]
parseBodyContents body =
  case runParser (bodyTokensParser <* MP.eof) "body" body of
    Left _ -> []
    Right tokens ->
      let (contents, _) = consumeLevel 0 tokens
      in contents

data BodyToken
  = HeadingToken Int String
  | ArrowListToken Int String
  | ImageToken String String String
  | TableToken [String] [[String]]
  | SummaryToken
  | FigureListToken
  | ReferencesToken
  | FooterToken Int String
  | CodeBlockToken [String]
  | BulletListToken [String]
  | BreakToken
  | ParagraphToken String
  deriving (Show, Eq)

bodyTokensParser :: BodyParser [BodyToken]
bodyTokensParser = do
  spaceConsumer
  many (blockToken <* spaceConsumer)
  where
  blockToken = headingToken
           <|> arrowListToken
           <|> imageToken
           <|> tableToken
           <|> summaryToken
           <|> figureListToken
           <|> referencesToken
           <|> footerToken
           <|> codeBlockToken
           <|> bulletListToken
           <|> breakToken
           <|> paragraphToken

headingToken :: BodyParser BodyToken
headingToken = try $ do
  _ <- takeWhileP (Just "heading indentation") isInlineSpace
  hashes <- some (char '#')
  _ <- char ' '
  rawTitle <- takeWhileP (Just "heading title") (/= '\n')
  _ <- optional newline
  let title = trim rawTitle
  guard (not (null title))
  pure (HeadingToken (length hashes) title)

arrowListToken :: BodyParser BodyToken
arrowListToken = try $ do
  _ <- takeWhileP (Just "arrow list indentation") isInlineSpace
  arrows <- some (char '>')
  guard (length arrows >= 2)
  _ <- char ' '
  rawTitle <- takeWhileP (Just "arrow list title") (/= '\n')
  _ <- optional newline
  let title = trim rawTitle
      level = max 1 (length arrows - 1)
  guard (not (null title))
  pure (ArrowListToken level title)

paragraphToken :: BodyParser BodyToken
paragraphToken = ParagraphToken . normalizeParagraphText <$> some paragraphLine

paragraphLine :: BodyParser String
paragraphLine = try $ do
  preview <- lookAhead (takeWhileP (Just "paragraph preview") (/= '\n'))
  guard (not (isHeadingLine preview))
  guard (not (isArrowLine preview))
  guard (not (isImageLine preview))
  guard (not (isTableLine preview))
  guard (not (isSummaryLine preview))
  guard (not (isFigureListLine preview))
  guard (not (isReferencesLine preview))
  guard (not (isFooterLine preview))
  guard (not (isBulletLine preview))
  guard (not (isCodeFenceLine preview))
  line <- takeWhileP (Just "paragraph line") (/= '\n')
  let trimmedLine = trim line
  guard (not (null trimmedLine))
  _ <- optional newline
  pure line

bulletListToken :: BodyParser BodyToken
bulletListToken = try $ do
  first <- lookAhead (takeWhileP (Just "bullet preview") (/= '\n'))
  guard (isBulletLine first)
  lines <- some $ try $ do
    preview <- lookAhead (takeWhileP (Just "bullet preview") (/= '\n'))
    guard (isBulletLine preview || isContinuationLine preview)
    line <- takeWhileP (Just "bullet line") (/= '\n')
    _ <- optional newline
    pure line
  pure (BulletListToken lines)

imageToken :: BodyParser BodyToken
imageToken = try $ do
  _ <- takeWhileP (Just "image indentation") isInlineSpace
  _ <- string "!["
  rawCaption <- takeWhileP (Just "image caption") (/= ']')
  _ <- string "]("
  rawPath <- takeWhileP (Just "image path") (/= ')')
  _ <- char ')'
  _ <- optional newline
  let imagePath = trim rawPath
      (captionDesc, captionSource) = splitImageCaption rawCaption
  guard (not (null imagePath))
  pure (ImageToken imagePath captionDesc captionSource)

summaryToken :: BodyParser BodyToken
summaryToken = try $ do
  raw <- takeWhileP (Just "summary line") (/= '\n')
  guard (isSummaryLine raw)
  _ <- optional newline
  pure SummaryToken

figureListToken :: BodyParser BodyToken
figureListToken = try $ do
  raw <- takeWhileP (Just "figure list line") (/= '\n')
  guard (isFigureListLine raw)
  _ <- optional newline
  pure FigureListToken

referencesToken :: BodyParser BodyToken
referencesToken = try $ do
  raw <- takeWhileP (Just "references line") (/= '\n')
  guard (isReferencesLine raw)
  _ <- optional newline
  pure ReferencesToken

tableToken :: BodyParser BodyToken
tableToken = try $ do
  first <- lookAhead (takeWhileP (Just "table preview") (/= '\n'))
  guard (isTableLine first)
  rows <- some $ try $ do
    preview <- lookAhead (takeWhileP (Just "table preview") (/= '\n'))
    guard (isTableLine preview)
    line <- takeWhileP (Just "table line") (/= '\n')
    _ <- optional newline
    pure (parseTableRow line)
  case rows of
    [] -> fail "empty table"
    [singleHeader] -> pure (TableToken singleHeader [])
    (header:bodyRows) -> pure (TableToken header bodyRows)

footerToken :: BodyParser BodyToken
footerToken = try $ do
  raw <- takeWhileP (Just "footer line") (/= '\n')
  case parseFooterLine raw of
    Nothing -> fail "not a footer tag"
    Just (pageNum, footerText) -> do
      _ <- optional newline
      pure (FooterToken pageNum footerText)

codeBlockToken :: BodyParser BodyToken
codeBlockToken = try $ do
  _ <- takeWhileP (Just "code fence indentation") isInlineSpace
  _ <- string "```"
  header <- takeWhileP (Just "code fence header") (/= '\n')
  -- If closing fence appears on the same line (inline fence like ```content```),
  -- capture content between the opening and closing markers and return it.
  let findClosing s = findIndex (isPrefixOf "```") (tails s)
  case findClosing header of
    Just idx ->
      let contentInline = take idx header
          contentLines = if null contentInline then [] else lines contentInline
      in pure (CodeBlockToken contentLines)
    Nothing -> do
      _ <- optional newline
      lines <- manyTill (takeWhileP (Just "code line") (/= '\n') <* optional newline) (try $ do
        _ <- takeWhileP (Just "code fence indentation") isInlineSpace
        _ <- string "```"
        _ <- optional newline
        pure ())
      pure (CodeBlockToken lines)

breakToken :: BodyParser BodyToken
breakToken = try $ do
  _ <- takeWhileP (Just "break indentation") isInlineSpace
  raw <- takeWhileP (Just "break") (/= '\n')
  _ <- optional newline
  let s = trim raw
  -- Do not treat as break if the raw line contains backticks (inline code/fence)
  guard (not ('`' `elem` raw) && (s == "___" || s == "---"))
  pure BreakToken

spaceConsumer :: BodyParser ()
spaceConsumer = MP.skipMany (blankLine <|> trailingSpaces)

blankLine :: BodyParser ()
blankLine = try $ do
  _ <- hspace
  _ <- newline
  pure ()

trailingSpaces :: BodyParser ()
trailingSpaces = try $ do
  _ <- hspace1
  MP.lookAhead MP.eof
  pure ()

isHeadingLine :: String -> Bool
isHeadingLine raw =
  let stripped = dropWhile isInlineSpace raw
      (hashes, rest) = span (== '#') stripped
  in not (null hashes) && case rest of
      (' ':xs) -> not (null (trim xs))
      _        -> False

isArrowLine :: String -> Bool
isArrowLine raw =
  let stripped = dropWhile isInlineSpace raw
      (arrows, rest) = span (== '>') stripped
  in length arrows >= 2 && case rest of
      (' ':xs) -> not (null (trim xs))
      _        -> False

isImageLine :: String -> Bool
isImageLine raw =
  let stripped = dropWhile isInlineSpace raw
  in "![" `isPrefix` stripped && "](" `contains` stripped && ")" `isSuffix` stripped
  where
    isPrefix pref s = take (length pref) s == pref
    isSuffix suf s = drop (length s - length suf) s == suf
    contains needle haystack = any (\suffix -> take (length needle) suffix == needle) (tails haystack)
    tails [] = [[]]
    tails xs@(_:rest) = xs : tails rest

isSummaryLine :: String -> Bool
isSummaryLine raw =
  trim raw == "::Summary::"

isFigureListLine :: String -> Bool
isFigureListLine raw =
  trim raw == "::FigureList::"

isReferencesLine :: String -> Bool
isReferencesLine raw =
  trim raw == "::References::"

isInlineSpace :: Char -> Bool
isInlineSpace c = c == ' ' || c == '\t'

consumeLevel :: Int -> [BodyToken] -> ([Content], [BodyToken])
consumeLevel _ [] = ([], [])
consumeLevel level tokens@(HeadingToken lvl _ : _)
  | lvl <= level = ([], tokens)
consumeLevel level tokens@(ArrowListToken lvl _ : _)
  | lvl <= level = ([], tokens)
consumeLevel level (HeadingToken lvl title : rest) =
  let (children, remaining) = consumeLevel lvl rest
      chapter = Chapter lvl title children
      (siblings, finalRest) = consumeLevel level remaining
  in (chapter : siblings, finalRest)
consumeLevel level (ArrowListToken lvl title : rest) =
  let (children, remaining) = consumeLevel lvl rest
      arrowList = ArrowList lvl title children
      (siblings, finalRest) = consumeLevel level remaining
  in (arrowList : siblings, finalRest)
consumeLevel level (ImageToken imagePath caption source : rest) =
  let imageNode = Figure (Image imagePath) (parseParagraph caption) (parseParagraph source)
      (siblings, finalRest) = consumeLevel level rest
  in (imageNode : siblings, finalRest)
consumeLevel level (TableToken headers rows : rest) =
  let tableNode = Table headers rows
      (siblings, finalRest) = consumeLevel level rest
  in (tableNode : siblings, finalRest)
consumeLevel level (SummaryToken : rest) =
  let (siblings, finalRest) = consumeLevel level rest
  in (Summary : siblings, finalRest)
consumeLevel level (FigureListToken : rest) =
  let (siblings, finalRest) = consumeLevel level rest
  in (FigureList : siblings, finalRest)
consumeLevel level (ReferencesToken : rest) =
  let (siblings, finalRest) = consumeLevel level rest
  in (References : siblings, finalRest)
consumeLevel level (FooterToken pageNum footerText : rest) =
  let footerNode = Footer pageNum (parseParagraph footerText)
      (siblings, finalRest) = consumeLevel level rest
  in (footerNode : siblings, finalRest)
consumeLevel level (BulletListToken lines : rest) =
  let items = parseBulletList lines
      (siblings, finalRest) = consumeLevel level rest
  in (items ++ siblings, finalRest)

consumeLevel level (CodeBlockToken lines : rest) =
  let codeText = unlines lines
      codeNode = CodeBlock codeText
      (siblings, remaining) = consumeLevel level rest
  in (codeNode : siblings, remaining)

consumeLevel level (BreakToken : rest) =
  let (siblings, remaining) = consumeLevel level rest
  in (Break : siblings, remaining)
consumeLevel level (ParagraphToken text : rest) =
  case paragraphNode text of
    Nothing -> consumeLevel level rest
    Just paragraph ->
      let (siblings, remaining) = consumeLevel level rest
      in (paragraph : siblings, remaining)

paragraphNode :: String -> Maybe Content
paragraphNode txt =
  case parseParagraph txt of
    [] -> Nothing
    writings -> Just (Paragraph writings)

parseParagraph :: String -> [Writing]
parseParagraph text =
  case runParser parseWritings "paragraph" text of
    Left _   -> [Plain text]
    Right [] -> [Plain text]
    Right ws -> ws

normalizeParagraphText :: [String] -> String
normalizeParagraphText =
  unwords . map trim . filter (not . null . trim)

parseBulletList :: [String] -> [Content]
parseBulletList lines =
  let items = splitBulletItems lines
      bulletItems = map (toBulletItem) items
  in [BulletList 1 bulletItems]

splitBulletItems :: [String] -> [[Content]]
splitBulletItems [] = []
splitBulletItems (line:rest) =
  if isBulletLine line
    then
      let (itemLines, remaining) = span (not . isBulletLine) rest
          -- remove bullet prefix from first line and dedent continuation lines
          first = stripBulletMarker line
          continuations = map stripContinuation itemLines
          itemStr = unlines (first : continuations)
          -- parse paragraph writings for the item
          ws = parseParagraph itemStr
          itemContent = [Paragraph ws]
      in itemContent : splitBulletItems remaining
    else splitBulletItems rest

isBulletLine :: String -> Bool
isBulletLine raw =
  let stripped = dropWhile isInlineSpace raw
  in case stripped of
      ('-':' ':_) -> True
      ('*':' ':_) -> True
      _           -> False

isContinuationLine :: String -> Bool
isContinuationLine raw =
  case raw of
    (c:_) -> isInlineSpace c
    _     -> False

stripBulletMarker :: String -> String
stripBulletMarker raw =
  let stripped = dropWhile isInlineSpace raw
  in case stripped of
    ('-':' ':xs) -> xs
    ('*':' ':xs) -> xs
    _ -> raw

stripContinuation :: String -> String
stripContinuation = dropWhile isInlineSpace

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

toBulletItem :: [Content] -> Content
toBulletItem cs =
  case cs of
    [Paragraph ws] -> Paragraph ws
    _ -> -- Flatten nested Paragraphs into a single Paragraph
      let ws = concat [ ws' | Paragraph ws' <- cs ]
      in Paragraph ws

isCodeFenceLine :: String -> Bool
isCodeFenceLine raw =
  let stripped = dropWhile isInlineSpace raw
  in take 3 stripped == "```"

isTableLine :: String -> Bool
isTableLine raw =
  let stripped = trim raw
  in not (null stripped)
      && head stripped == '['
      && last stripped == ']'
      && '|' `elem` stripped

isFooterLine :: String -> Bool
isFooterLine raw =
  case parseFooterLine raw of
    Just _ -> True
    Nothing -> False

parseFooterLine :: String -> Maybe (Int, String)
parseFooterLine raw =
  let stripped = trim raw
      prefixes = ["::Footer(", "::footer(", "::Rodape(", "::rodape(", "::Rodapé(", "::rodapé("]
      parseWithPrefix pref =
        if pref `isPrefixOf` stripped
          then
            let rest = drop (length pref) stripped
                (digits, restAfterDigits) = span isDigit rest
            in case (digits, restAfterDigits) of
                ("", _) -> Nothing
                (_, ')':':':':':txt) ->
                  let pageNum = read digits
                      footerText = trim txt
                  in if pageNum > 0 && not (null footerText)
                      then Just (pageNum, footerText)
                      else Nothing
                _ -> Nothing
          else Nothing
  in foldr (\pref acc -> case acc of
                    Just _ -> acc
                    Nothing -> parseWithPrefix pref
            ) Nothing prefixes

parseTableRow :: String -> [String]
parseTableRow raw =
  let stripped = trim raw
      inner = init (tail stripped)
  in map trim (splitByPipe inner)

splitByPipe :: String -> [String]
splitByPipe [] = [""]
splitByPipe (c:cs)
  | c == '|' = "" : splitByPipe cs
  | otherwise =
      case splitByPipe cs of
        [] -> [[c]]
        (x:xs) -> (c:x) : xs

splitImageCaption :: String -> (String, String)
splitImageCaption raw =
  let parts = splitByPipe raw
  in case parts of
      [] -> ("", "")
      [onlyCaption] -> (trim onlyCaption, "")
      (caption:rest) -> (trim caption, trim (concatWithPipe rest))
  where
    concatWithPipe [] = ""
    concatWithPipe [x] = x
    concatWithPipe (x:xs) = x ++ "|" ++ concatWithPipe xs
