{-# LANGUAGE OverloadedStrings #-}
module Parsers.MainTags where

import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr
import Data.Text hiding (empty)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Control.Monad (when, void)
import Data.Char            (digitToInt)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Map.Strict as M

import System.IO.Unsafe (unsafePerformIO)

import System.FilePath (takeExtension)

import Ast.AbstractSyntaxTree
import Parsers.Paragraphs

parseMainContent :: Parser MainSection
parseMainContent =  parseCommentary <|> parseNumberedList <|> parseBulletList <|> parseLetteredList <|> parseMathBlock <|> parseCentered <|> parseAbreviations <|> parseRightContent <|> parseAbstract <|> parseThanks <|> parseReferences <|> parseFigureList <|> parseTable <|> parseQuote <|> parseChap <|> parseSummary <|> parseRef <|> parseList <|> parseLink <|> parseTrace <|> parseImageUrl <|> parseImage <|> parseVideo <|> parseAudio <|> parseCode <|> parseMeta <|> parseContent

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
  void (string "(nl)")
  void eol
  items <- manyTill parseListItem (void $ string "(/nl)")
  return $ NumberedList items
  where
    parseListItem :: Parser [MainSection]
    parseListItem = do
      skipMany (char ' ' <|> char '\t')
      void (L.decimal >> char '.' >> space1)
      tags <- manyTill parseContent $
           lookAhead (void eol)
        <|> lookAhead (void $ L.decimal >> char '.' >> space1)
        <|> lookAhead (void $ string "(/nl)")
      void eol <|> void eof
      return tags

parseBulletList :: Parser MainSection
parseBulletList = do
  void (string "(bl)")
  void eol
  items <- manyTill parseListItem (void $ string "(/bl)")
  return $ BulletList items
  where
    parseListItem :: Parser [MainSection]
    parseListItem = do
      skipMany (char ' ' <|> char '\t')
      void (char '-' >> space1)
      tags <- manyTill parseContent $
           lookAhead (void eol)
        <|> lookAhead (void $ char '-' >> space1)
        <|> lookAhead (void $ string "(/bl)")
      void eol <|> void eof
      return tags

parseLetteredList :: Parser MainSection
parseLetteredList = do
  void (string "(ll)" <* eol)
  items <- manyTill parseListItem (string "(/ll)" >> eol)
  return $ LetteredList items
 where
  parseListItem :: Parser [MainSection]
  parseListItem = do
    skipMany (char ' ' <|> char '\t')
    -- só aceita 'a', 'b', …, 'z' antes do ')'
    void (lowerChar >> char ')' >> space1)
    tags <- manyTill parseContent $
         lookAhead (void eol)
      <|> lookAhead (void $ lowerChar >> char ')' >> space1)
      <|> lookAhead (void $ string "(/ll)")
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
    when (Prelude.length t > 64) $
      fail $ "chap tag title is longer than 64 characters: " ++ show t
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
    when (Prelude.length c > 64) $
      fail $ "localimg tag content is longer than 64 characters: " ++ c
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
    when (Prelude.length c > 64) $
      fail $ "img tag content is longer than 64 characters: " ++ c
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
parseMetaContent = parseAuthor <|> parseInstitution <|> parseSubtitle <|> parseLocation <|> parseYear <|> parseDescription <|> parseMentor

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

parseMentor :: Parser MetaSection
parseMentor = do
  _ <- string "(mentor)"
  _ <- many (char ' ' <|> char '\n')
  content <- manyTill anySingle (string "(/mentor)")
  _ <- many (char ' ' <|> char '\n')
  return (Mentor content)

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

parseThanks :: Parser MainSection
parseThanks = do
    _ <- string "(thanks)"
    _ <- many (char ' ' <|> char '\n')
    content <- manyTill parseMainContent (string "(/thanks)")
    return (Thanks content)

parseAbstract :: Parser MainSection
parseAbstract = do
    _ <- string "(abstract |"
    title <- manyTill anySingle (string ")")
    content <- manyTill parseMainContent (string "(/abstract)")
    return (Abstract title content)


parseAbreviations :: Parser MainSection
parseAbreviations = do
    _     <- string "(abbreviations |"
    title <- manyTill anySingle (char ')')
    void eol
    content <- manyTill parseAbbr
                  (string "(/abbreviations)" >> (void eol <|> pure ()))
    return (Abbreviations title content)
  where
    parseAbbr :: Parser Abbr
    parseAbbr = do
      skipMany (char ' ' <|> char '\t')
      -- lê tudo até o '-' como SIGLA
      sigla <- manyTill anySingle (lookAhead (char '-'))
      char '-' >> space1
      -- lê tudo até o fim da linha ou fechamento
      significado <- manyTill anySingle
           (   lookAhead (void eol)
           <|> lookAhead (void $ string "(/abbreviations)")
           )
      void eol <|> void eof
      return $ Abbr (sigla) (significado)

-- Math Parser
sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parensP :: Parser MathExpr
parensP = Parens <$> between (symbol "(") (symbol ")") parseExpr

parens, brackets :: Parser a -> Parser a
parens   = between (symbol "(") (symbol ")")
brackets = between (symbol "[") (symbol "]")

operatorTable :: [[Operator Parser MathExpr]]
operatorTable =
  [ [ Prefix  (Neg <$ symbol "-") ]
  , [ InfixL  (ImplicitMul <$ symbol "&")
    , InfixL  (Mul         <$ symbol "*")
    , InfixL  (Div         <$ symbol "/") ]
  , [ InfixL  (Add         <$ symbol "+")
    , InfixL  (Sub <$ lexeme (string "-" <* notFollowedBy (char '>')))
    ]
  , [ InfixN  (Arrow <$ symbol "->" <|> Arrow <$ symbol "→" <|> Arrow <$ symbol "to" )
    , InfixN  ( Eq    <$ symbol "=" )
    ]
  ]

term :: Parser MathExpr
term =
      try parseFunction
  <|> try parseCall
  <|> parensP
  <|> Ellipsis
      <$  lexeme (string "...")
  <|> Var    <$> lexeme ((:) <$> letterChar <*> many (letterChar <|> digitChar <|> char '_'))
  <|> Number <$> lexeme (some digitChar)

parseCall :: Parser MathExpr
parseCall = do
  nome <- lexeme ((:) <$> letterChar <*> many (letterChar <|> digitChar <|> char '_'))
  args <- between (symbol "(") (symbol ")")
                  (parseExpr `sepBy` symbol ",")
  return (Func nome args)

parseExpr :: Parser MathExpr
parseExpr = makeExprParser term operatorTable

parseFunction :: Parser MathExpr
parseFunction = brackets $ do
  name <- lexeme $ choice $ Prelude.map string
    [ "fraction", "power-of", "square-root", "prob", "sum",
      "prod", "integral", "lim", "deriv", "root", "binom", "abs",
      "vector", "matrix", "func", "cases" ]
  _    <- symbol "|"
  args <- parseExpr `sepBy1` symbol "|"
  case (name, args) of
    ("fraction",    [n, d]) -> pure (Fraction   n d)
    ("power-of",    [b, e]) -> pure (PowerOf    b e)
    ("square-root", [x    ]) -> pure (SquareRoot x)
    ("prob",        [evt, c]) -> pure (Probability evt c)
    ("sum",    [i0, iN, body])     -> pure (Sum     i0 iN body)
    ("prod",   [i0, iN, body])     -> pure (Product i0 iN body)
    ("integral",    [body])             -> pure (Integral Nothing        body)
    ("integral",    [a, b, body])       -> pure (Integral (Just (a,b))   body)
    ("lim",    [at, body])         -> pure (Limit      at     body)
    ("deriv",  [d,  body])         -> pure (Derivative d      body)
    ("root",   [n,  x])            -> pure (Root (Just n)        x)
    ("root",   [x   ])             -> pure (Root Nothing         x)
    ("binom",  [n, k])             -> pure (Binom n k)
    ("abs",    [x])                -> pure (Abs x)
    ("vector",    xs@(_:_))           -> pure (Vector xs)
    ("matrix",    rows@(_:_))         ->
      let toRow (Vector r) = r
          toRow other      = [other]
      in pure (Matrix (Prelude.map toRow rows))

    ("func",   (Var f):xs)         -> pure (Func f xs)
    ("cases",  pairs)
      | even (Prelude.length pairs)        ->
        let mk ((e:c:rest)) = (e,c) : mk rest
            mk []             = []
            mk _              = error "odd args to cases"
        in pure (Piecewise (mk pairs))
    _ -> fail $ "wrong args for math func " ++ name

parseMathBlock :: Parser MainSection
parseMathBlock =
  MathBlock . pure
    <$> between
          (symbol "(math)")
          (symbol "(/math)")
          parseExpr