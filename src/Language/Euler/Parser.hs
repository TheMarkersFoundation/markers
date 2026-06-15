module Language.Euler.Parser (parseEuler, parseStmt) where

import Data.Void (Void)
import Language.Euler.Ast.Block (Block, Stmt (..))
import Language.Euler.Ast.Expr (Expr (..), Op (..))
import Language.Euler.Symbols (isFunction)
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, char, digitChar, letterChar, string)

{-

    Euler's parser. Each line of a !math(...) block is parsed independently as a
    single statement, which keeps the grammar tiny and makes a typo on one line
    harmless to the others. Expressions use a small precedence ladder:

        relation  (= != <= >= < >)        -- loosest
        additive  (+ - +-)
        term      (* /, and juxtaposition like 2a)
        unary     (leading -)
        scripted  (postfix ^ and _)
        atom      (number | (expr) | sqrt/func | identifier)   -- tightest

-}

type Parser = Parsec Void String

-- | Parse a whole block (the raw text inside one !math(...) tag). Blank lines
--   are ignored; any non-blank line that fails to parse fails the whole block.
parseEuler :: String -> Either String Block
parseEuler = traverse parseStmt . filter (not . isBlank) . map stripCR . lines
  where
    isBlank = all (`elem` " \t")

-- | Parse a single statement line.
parseStmt :: String -> Either String Stmt
parseStmt line =
  case runParser (hspaces *> statement <* eof) "euler" line of
    Left err -> Left (errorBundlePretty err)
    Right s -> Right s

stripCR :: String -> String
stripCR s = case reverse s of
  ('\r' : rest) -> reverse rest
  _ -> s

-- Lexing helpers -------------------------------------------------------------

hspaces :: Parser ()
hspaces = skipMany (char ' ' <|> char '\t')

lexeme :: Parser a -> Parser a
lexeme p = p <* hspaces

symb :: String -> Parser String
symb s = lexeme (string s)

keyword :: String -> Parser String
keyword s = lexeme (try (string s <* notFollowedBy alphaNumChar))

-- Statements -----------------------------------------------------------------

statement :: Parser Stmt
statement = try define <|> (Express <$> expr)

define :: Parser Stmt
define = do
  _ <- optional (keyword "def")
  name <- lexeme identifier
  _ <- symb ":="
  Define name <$> expr

-- Expressions ----------------------------------------------------------------

expr :: Parser Expr
expr = relation

relation :: Parser Expr
relation = chain additive relationOp

additive :: Parser Expr
additive = chain term additiveOp

-- | Multiplicative level, including division (as a fraction) and implicit
--   multiplication by juxtaposition (e.g. 2a, 4ac, a(b+c)).
term :: Parser Expr
term = unary >>= go
  where
    go acc =
          (symb "*" >> unary >>= \y -> go (Bin Times acc y))
      <|> (symb "/" >> unary >>= \y -> go (Frac acc y))
      <|> (try scripted >>= \y -> go (Bin Times acc y))
      <|> pure acc

unary :: Parser Expr
unary = (symb "-" >> (Neg <$> unary)) <|> scripted

scripted :: Parser Expr
scripted = atom >>= go
  where
    go acc =
          (symb "^" >> scriptArg >>= \e -> go (Superscript acc e))
      <|> (symb "_" >> scriptArg >>= \e -> go (Subscript acc e))
      <|> pure acc
    -- A script binds to the following unary factor; use parentheses to group
    -- anything larger, e.g. x^(n+1). Allows negative exponents like x^-1.
    scriptArg = unary

atom :: Parser Expr
atom =
      lexeme number
  <|> parens
  <|> matrixP
  <|> absP
  <|> ellipsis
  <|> funcOrVar

number :: Parser Expr
number = do
  whole <- some digitChar
  frac <- optional (try (char '.' *> some digitChar))
  pure (Num (whole ++ maybe "" ('.' :) frac))

parens :: Parser Expr
parens = Group <$> (symb "(" *> expr <* symb ")")

-- A matrix / vector: [a, b; c, d]. Rows are separated by ';', cells by ','.
matrixP :: Parser Expr
matrixP = do
  _ <- symb "["
  rows <- row `sepBy1` symb ";"
  _ <- symb "]"
  pure (Matrix rows)
  where
    row = expr `sepBy1` symb ","

-- Absolute value: |x|.
absP :: Parser Expr
absP = Abs <$> (symb "|" *> expr <* symb "|")

-- The ellipsis '...' renders as the midline horizontal ellipsis ⋯.
ellipsis :: Parser Expr
ellipsis = try (symb "...") >> pure (Symbol "\8943")

funcOrVar :: Parser Expr
funcOrVar = do
  name <- lexeme identifier
  if isFunction name
    then funcCall name <|> pure (Var name)
    else pure (Var name)
  where
    funcCall name = do
      args <- symb "(" *> (expr `sepBy` symb ",") <* symb ")"
      pure (mkFunc name args)

-- 'sqrt(x)' becomes a dedicated Sqrt node; other functions stay generic.
mkFunc :: String -> [Expr] -> Expr
mkFunc "sqrt" [a] = Sqrt a
mkFunc name args = Func name args

identifier :: Parser String
identifier = do
  c <- letterChar
  cs <- many alphaNumChar
  pure (c : cs)

-- Operator parsers -----------------------------------------------------------

relationOp :: Parser (Expr -> Expr -> Expr)
relationOp = lexeme $
      (try (string "<=>") >> pure (Bin LeftRightDArrow))
  <|> (try (string "<->") >> pure (Bin LeftRightArrow))
  <|> (try (string "->")  >> pure (Bin RightArrow))
  <|> (try (string "=>")  >> pure (Bin RightDArrow))
  <|> (try (string "!=")  >> pure (Bin Neq))
  <|> (try (string "<=")  >> pure (Bin Le))
  <|> (try (string ">=")  >> pure (Bin Ge))
  <|> (char '=' >> pure (Bin Eq))
  <|> (char '<' >> pure (Bin Lt))
  <|> (char '>' >> pure (Bin Gt))

additiveOp :: Parser (Expr -> Expr -> Expr)
additiveOp = lexeme $
      (try (string "+-") >> pure (Bin PlusMinus))
  <|> (char '+' >> pure (Bin Add))
  -- A bare '-' is subtraction, but '->' is the arrow operator, so don't let
  -- subtraction swallow the dash of an arrow.
  <|> (try (char '-' <* notFollowedBy (char '>')) >> pure (Bin Minus))

-- | Left-associative binary operator chain.
chain :: Parser Expr -> Parser (Expr -> Expr -> Expr) -> Parser Expr
chain p op = p >>= rest
  where
    rest acc = (op >>= \f -> p >>= \y -> rest (f acc y)) <|> pure acc
