module Language.Euler.Converter.Html
  ( renderEuler
  , renderInline
  , renderBlock
  , renderStmt
  , renderExpr
  , eulerCss
  ) where

import Data.List (intercalate)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Language.Euler.Ast.Block (Block, Stmt (..))
import Language.Euler.Ast.Expr (Expr (..), Op)
import Language.Euler.Env (Env)
import Language.Euler.Parser (parseStmt)
import Language.Euler.Symbols (operatorSymbol, symbol)

{-

    Render Euler to self-contained HTML. The markup leans on a tiny stylesheet
    ('eulerCss') for fractions, roots, matrices and operator spacing, so it looks
    right both in the live preview and in the ABNT print layout. Lines that fail
    to parse are shown verbatim instead of blanking the block.

    Rendering takes an 'Env' of definitions: a reference to a defined name is
    expanded to the expression it stands for (so 'A' defined as 'pi*r^2' renders
    as π·r² wherever it is used). Definition lines themselves are shown literally.

-}

-- | Render the raw text of one !math(...) block, line by line (display math).
renderEuler :: Env -> String -> String
renderEuler env src =
  case nonBlankLines src of
    [] -> ""
    ls -> concatMap (renderDisplayLine env) ls

-- | Render the raw text of an inline !math(...) span (flows within a paragraph).
renderInline :: Env -> String -> String
renderInline env src =
  case nonBlankLines src of
    [] -> ""
    ls ->
      "<span class=\"euler euler-inline\">"
        ++ intercalate "<span class=\"euler-inline-sep\"> </span>" (map (renderInlineLine env) ls)
        ++ "</span>"

-- | Render an already-parsed block as display math.
renderBlock :: Env -> Block -> String
renderBlock env = concatMap (wrapLine . renderStmt env)

renderDisplayLine :: Env -> String -> String
renderDisplayLine env line =
  case parseStmt line of
    Right stmt -> wrapLine (renderStmt env stmt)
    Left _ -> "<span class=\"euler-line euler-error\">" ++ escape line ++ "</span>"

renderInlineLine :: Env -> String -> String
renderInlineLine env line =
  case parseStmt line of
    Right stmt -> renderStmt env stmt
    Left _ -> "<span class=\"euler-error\">" ++ escape line ++ "</span>"

wrapLine :: String -> String
wrapLine inner = "<span class=\"euler-line\">" ++ inner ++ "</span>"

renderStmt :: Env -> Stmt -> String
-- A reference inside a standalone expression is expanded through the env.
renderStmt env (Express e) = render (expand env e)
-- A definition shows its right-hand side literally (not expanded).
renderStmt _ (Define name e) =
  "<span class=\"euler-defname\">" ++ renderName name ++ "</span>"
    ++ "<span class=\"euler-op euler-defeq\">\8788</span>" -- ≔
    ++ render e

-- | Render an expression, expanding definitions first.
renderExpr :: Env -> Expr -> String
renderExpr env = render . expand env

-- Expansion ------------------------------------------------------------------

-- | Replace every reference to a defined name with the expression it stands
--   for, recursively. A 'seen' set breaks cycles (e.g. mutually recursive
--   definitions) so expansion always terminates.
expand :: Env -> Expr -> Expr
expand env = go S.empty
  where
    go seen e = case e of
      Var name
        | Just def <- M.lookup name env
        , not (S.member name seen) -> maybeGroup (go (S.insert name seen) def)
        | otherwise -> e
      Num _ -> e
      Symbol _ -> e
      Neg x -> Neg (go seen x)
      Bin op a b -> Bin op (go seen a) (go seen b)
      Superscript a b -> Superscript (go seen a) (go seen b)
      Subscript a b -> Subscript (go seen a) (go seen b)
      Frac a b -> Frac (go seen a) (go seen b)
      Sqrt x -> Sqrt (go seen x)
      Abs x -> Abs (go seen x)
      Matrix rows -> Matrix (map (map (go seen)) rows)
      Func n args -> Func n (map (go seen) args)
      Group x -> Group (go seen x)
    -- Keep precedence safe when a name expands to a compound expression.
    maybeGroup x = case x of
      Bin{} -> Group x
      Neg{} -> Group x
      _ -> x

-- Pure rendering (post-expansion) --------------------------------------------

render :: Expr -> String
render (Num n) = "<span class=\"euler-num\">" ++ escape n ++ "</span>"
render (Var v) = renderName v
render (Symbol s) = "<span class=\"euler-sym\">" ++ s ++ "</span>"
render (Neg e) = "<span class=\"euler-op euler-neg\">\8722</span>" ++ render e
render (Bin op a b) = render a ++ renderOp op ++ render b
render (Superscript b e) = render b ++ "<sup>" ++ render e ++ "</sup>"
render (Subscript b ix) = render b ++ "<sub>" ++ render ix ++ "</sub>"
render (Frac n d) =
  "<span class=\"euler-frac\">"
    ++ "<span class=\"euler-num-part\">" ++ render (unGroup n) ++ "</span>"
    ++ "<span class=\"euler-den-part\">" ++ render (unGroup d) ++ "</span>"
    ++ "</span>"
render (Sqrt e) =
  "<span class=\"euler-sqrt\">\8730"
    ++ "<span class=\"euler-radicand\">" ++ render (unGroup e) ++ "</span>"
    ++ "</span>"
render (Abs e) =
  "<span class=\"euler-abs\"><span class=\"euler-fence\">|</span>"
    ++ render (unGroup e)
    ++ "<span class=\"euler-fence\">|</span></span>"
render (Matrix rows) =
  "<span class=\"euler-matrix\">" ++ concatMap renderRow rows ++ "</span>"
  where
    renderRow cells = "<span class=\"euler-matrix-row\">" ++ concatMap renderCell cells ++ "</span>"
    renderCell c = "<span class=\"euler-matrix-cell\">" ++ render c ++ "</span>"
render (Func name args) = renderFunc name args
render (Group e) =
  "<span class=\"euler-paren\">(</span>" ++ render e ++ "<span class=\"euler-paren\">)</span>"

-- Function application, with special "utensils" for accents and brackets.
renderFunc :: String -> [Expr] -> String
renderFunc "abs" [x] = render (Abs x)
renderFunc "norm" [x] = fenced "\8214" x          -- ‖x‖
renderFunc "floor" [x] = bracketed "\8970" "\8971" x  -- ⌊x⌋
renderFunc "ceil" [x] = bracketed "\8968" "\8969" x   -- ⌈x⌉
renderFunc "vec" [x] = accent "\8407" x           -- combining right arrow above
renderFunc "hat" [x] = accent "\770" x            -- combining circumflex
renderFunc "dot" [x] = accent "\775" x            -- combining dot above
renderFunc "tilde" [x] = accent "\771" x          -- combining tilde
renderFunc "bar" [x] = "<span class=\"euler-overline\">" ++ render (unGroup x) ++ "</span>"
renderFunc name args =
  "<span class=\"euler-func\">" ++ escape name ++ "</span>"
    ++ "<span class=\"euler-paren\">(</span>"
    ++ intercalate "<span class=\"euler-comma\">, </span>" (map render args)
    ++ "<span class=\"euler-paren\">)</span>"

accent :: String -> Expr -> String
accent mark x = "<span class=\"euler-accent\">" ++ render (unGroup x) ++ mark ++ "</span>"

fenced :: String -> Expr -> String
fenced bar x =
  "<span class=\"euler-abs\"><span class=\"euler-fence\">" ++ bar ++ "</span>"
    ++ render (unGroup x)
    ++ "<span class=\"euler-fence\">" ++ bar ++ "</span></span>"

bracketed :: String -> String -> Expr -> String
bracketed open close x =
  "<span class=\"euler-fence\">" ++ open ++ "</span>"
    ++ render (unGroup x)
    ++ "<span class=\"euler-fence\">" ++ close ++ "</span>"

-- | A defined / variable name, mapped to its symbol when it names one.
renderName :: String -> String
renderName name =
  case symbol name of
    Just glyph -> "<span class=\"euler-sym\">" ++ glyph ++ "</span>"
    Nothing -> "<span class=\"euler-var\">" ++ escape name ++ "</span>"

renderOp :: Op -> String
renderOp op = "<span class=\"euler-op\">" ++ operatorSymbol op ++ "</span>"

-- Drop one layer of parentheses so fractions, roots and accents don't double up.
unGroup :: Expr -> Expr
unGroup (Group e) = e
unGroup e = e

nonBlankLines :: String -> [String]
nonBlankLines = filter (not . isBlank) . map stripCR . lines
  where
    isBlank = all (`elem` " \t")

escape :: String -> String
escape = concatMap esc
  where
    esc '&' = "&amp;"
    esc '<' = "&lt;"
    esc '>' = "&gt;"
    esc '"' = "&quot;"
    esc '\'' = "&#39;"
    esc c = [c]

stripCR :: String -> String
stripCR s = case reverse s of
  ('\r' : rest) -> reverse rest
  _ -> s

-- | Stylesheet shared by every renderer that embeds Euler output.
eulerCss :: String
eulerCss = unlines
  [ ".euler-block { text-align: center; margin: 1em 0; }"
  , ".euler-line { display: block; line-height: 2; }"
  , ".euler-line + .euler-line { margin-top: 0.35em; }"
  , ".euler-inline { white-space: nowrap; }"
  , ".euler-inline-sep { margin: 0 0.35em; }"
  , ".euler-var, .euler-sym, .euler-defname { font-style: italic; }"
  , ".euler-num { font-style: normal; }"
  , ".euler-op { margin: 0 0.28em; }"
  , ".euler-neg { margin: 0 0.05em 0 0; }"
  , ".euler-func { font-style: normal; margin-right: 0.05em; }"
  , ".euler-frac { display: inline-flex; flex-direction: column; vertical-align: middle; text-align: center; margin: 0 0.2em; }"
  , ".euler-frac > .euler-num-part { border-bottom: 1px solid currentColor; padding: 0 0.35em; }"
  , ".euler-frac > .euler-den-part { padding: 0.05em 0.35em 0; }"
  , ".euler-sqrt { white-space: nowrap; }"
  , ".euler-sqrt > .euler-radicand { border-top: 1px solid currentColor; padding: 0 0.2em; }"
  , ".euler-overline { border-top: 1px solid currentColor; }"
  , ".euler-fence { margin: 0 0.05em; }"
  , ".euler-matrix { display: inline-flex; flex-direction: column; vertical-align: middle; position: relative; margin: 0 0.2em; padding: 0.15em 0.5em; }"
  , ".euler-matrix::before, .euler-matrix::after { content: \"\"; position: absolute; top: 0; bottom: 0; width: 0.35em; border: 1px solid currentColor; }"
  , ".euler-matrix::before { left: 0; border-right: 0; }"
  , ".euler-matrix::after { right: 0; border-left: 0; }"
  , ".euler-matrix-row { display: flex; justify-content: center; gap: 0.7em; padding: 0.08em 0; }"
  , ".euler-matrix-cell { text-align: center; }"
  , ".euler-error { color: #b00020; font-family: monospace; }"
  ]
