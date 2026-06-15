module Language.Euler.Symbols (symbol, operatorSymbol, isFunction) where

import qualified Data.Map.Strict as M
import Language.Euler.Ast.Expr (Op (..))

{-

    Lookup tables that turn plain ASCII names into mathematical symbols. The
    parser keeps identifiers verbatim; the conversion to a glyph happens here at
    render time, so the AST stays readable and language-agnostic.

-}

-- | The glyph for an identifier, when it names a known symbol (greek letters
--   and a few common constants/operators). Anything else renders unchanged.
symbol :: String -> Maybe String
symbol name = M.lookup name symbolTable

symbolTable :: M.Map String String
symbolTable = M.fromList (greekLower ++ greekUpper ++ extras)
  where
    greekLower =
      [ ("alpha", "\945"), ("beta", "\946"), ("gamma", "\947")
      , ("delta", "\948"), ("epsilon", "\949"), ("zeta", "\950")
      , ("eta", "\951"), ("theta", "\952"), ("iota", "\953")
      , ("kappa", "\954"), ("lambda", "\955"), ("mu", "\956")
      , ("nu", "\957"), ("xi", "\958"), ("omicron", "\959")
      , ("pi", "\960"), ("rho", "\961"), ("sigma", "\963")
      , ("tau", "\964"), ("upsilon", "\965"), ("phi", "\966")
      , ("chi", "\967"), ("psi", "\968"), ("omega", "\969")
      ]
    greekUpper =
      [ ("Gamma", "\915"), ("Delta", "\916"), ("Theta", "\920")
      , ("Lambda", "\923"), ("Xi", "\926"), ("Pi", "\928")
      , ("Sigma", "\931"), ("Phi", "\934"), ("Psi", "\936")
      , ("Omega", "\937")
      ]
    extras =
      [ ("inf", "\8734"), ("infty", "\8734"), ("infinity", "\8734")
      , ("pm", "\177"), ("times", "\215"), ("cdot", "\183")
      , ("deg", "\176"), ("partial", "\8706"), ("nabla", "\8711")
      , ("forall", "\8704"), ("exists", "\8707"), ("in", "\8712")
      , ("notin", "\8713"), ("emptyset", "\8709"), ("sum", "\8721")
      , ("prod", "\8719"), ("int", "\8747"), ("approx", "\8776")
      , ("propto", "\8733"), ("neq", "\8800")
      ]

-- | Rendering of a binary operator glyph. Spacing is handled by CSS.
operatorSymbol :: Op -> String
operatorSymbol Add = "+"
operatorSymbol Minus = "\8722"      -- − minus sign
operatorSymbol PlusMinus = "\177"   -- ±
operatorSymbol Times = "\183"       -- · middle dot
operatorSymbol Eq = "="
operatorSymbol Neq = "\8800"        -- ≠
operatorSymbol Le = "\8804"         -- ≤
operatorSymbol Ge = "\8805"         -- ≥
operatorSymbol Lt = "<"
operatorSymbol Gt = ">"
operatorSymbol RightArrow = "\8594"      -- →
operatorSymbol RightDArrow = "\8658"     -- ⇒
operatorSymbol LeftRightArrow = "\8596"  -- ↔
operatorSymbol LeftRightDArrow = "\8660" -- ⇔

-- | Identifiers that the parser treats as function names: 'name(args)' is a
--   function application rather than implicit multiplication. Everything else
--   followed by parentheses is read as multiplication, e.g. a(b+c) = a·(b+c).
isFunction :: String -> Bool
isFunction name = name `elem` knownFunctions

knownFunctions :: [String]
knownFunctions =
  [ "sqrt", "sin", "cos", "tan", "cot", "sec", "csc"
  , "sinh", "cosh", "tanh", "log", "ln", "exp", "abs"
  , "lim", "max", "min", "gcd", "lcm", "det", "dim"
  -- "utensils": accents and bracket operators rendered specially
  , "vec", "hat", "bar", "dot", "tilde", "floor", "ceil", "norm"
  ]
