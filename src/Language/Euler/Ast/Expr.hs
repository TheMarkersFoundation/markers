module Language.Euler.Ast.Expr where

{-

    Euler is Markers' small mathematical notation language. It is intentionally
    far simpler than LaTeX: a handful of ASCII operators, named greek letters,
    powers, indices, fractions, roots, matrices and accents are enough to
    typeset most academic formulae.

    Expr is the abstract syntax of a single mathematical expression. Whole
    statements (definitions and equations) live in Language.Euler.Ast.Block.

-}

-- | Binary operators. Division is *not* an operator here: 'a / b' is parsed
--   into a stacked 'Frac' so that it renders as a real fraction.
data Op
  = Add            -- ^ +
  | Minus          -- ^ - (binary subtraction)
  | PlusMinus      -- ^ +-  (rendered as ±)
  | Times          -- ^ *   (rendered as ·), also produced by juxtaposition (2a)
  | Eq             -- ^ =
  | Neq            -- ^ !=  (rendered as ≠)
  | Le             -- ^ <=  (rendered as ≤)
  | Ge             -- ^ >=  (rendered as ≥)
  | Lt             -- ^ <
  | Gt             -- ^ >
  | RightArrow     -- ^ ->  (→)
  | RightDArrow    -- ^ =>  (⇒)
  | LeftRightArrow -- ^ <-> (↔)
  | LeftRightDArrow -- ^ <=> (⇔)
  deriving (Show, Eq)

-- | A single Euler expression.
data Expr
  = Num String          -- ^ numeric literal, kept verbatim ("3.14")
  | Var String          -- ^ identifier ("x", "alpha", "A"); greek names become symbols at render time
  | Symbol String       -- ^ a literal glyph already chosen by the parser (e.g. the ellipsis ⋯)
  | Bin Op Expr Expr    -- ^ binary operation
  | Neg Expr            -- ^ unary minus
  | Superscript Expr Expr -- ^ power / superscript (base, exponent)
  | Subscript Expr Expr   -- ^ subscript (base, index)
  | Frac Expr Expr      -- ^ stacked fraction (numerator, denominator)
  | Sqrt Expr           -- ^ square root
  | Abs Expr            -- ^ absolute value, written |x|
  | Matrix [[Expr]]     -- ^ matrix / vector, written [a, b; c, d] (rows split by ;)
  | Func String [Expr]  -- ^ named function application, e.g. sin(x), vec(v), floor(x)
  | Group Expr          -- ^ parenthesised sub-expression
  deriving (Show, Eq)
