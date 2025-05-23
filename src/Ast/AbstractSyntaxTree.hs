module Ast.AbstractSyntaxTree where

data TextTag = Default String 
            | Crossed String
            | Underlined String
            | Bold String
            | Italic String
            | CodeInline String
            | BoldItalic String
            | Small String
            | Top String
            | Color String String
            deriving (Show)

data MetaSection = Author String
                 | Institution String
                 | Subtitle String
                 | Location String
                 | Year String
                 | Description String
                 deriving (Show)

data Abbr = Abbr String String
  deriving (Show, Eq)

data MainSection =  
                Empty
                | Centered [MainSection]
                | RightContent [MainSection]
                | Highlighted String [MainSection]
                | NumberedList [[MainSection]]
                | Paragraph TextTag
                | Meta [MetaSection]
                | List String [MainSection]
                | Chap String [MainSection]
                | Ref String String String String String [MainSection]
                | Link String [MainSection]
                | Trace String [MainSection]
                | Image String String [MainSection]
                | ImageUrl String [MainSection]
                | ImagePage String String String [MainSection]
                | ImageUrlPage String String [MainSection]
                | Video String [MainSection]
                | Audio String [MainSection]
                | Table [String] [[String]]
                | Code [MainSection]
                | Quote String [MainSection]
                | Abntchapter String String [MainSection]
                | Commentary String
                | LineBreak
                | Tab
                | Separator
                | References
                | Figurelist
                | MathBlock [MathExpr]
                | Summary String
                | Thanks [MainSection]
                | Abstract String [MainSection]
                | Abbreviations String [Abbr]
                deriving (Show)

data MathExpr
  = Number String
  | Add MathExpr MathExpr
  | Sub MathExpr MathExpr
  | Mul MathExpr MathExpr
  | ImplicitMul MathExpr MathExpr
  | Div MathExpr MathExpr
  | PowerOf    MathExpr MathExpr
  | SquareRoot MathExpr
  | Fraction   MathExpr MathExpr
  | Var String
  | Eq         MathExpr MathExpr
  | Arrow      MathExpr MathExpr
  | Neg MathExpr
  | Probability MathExpr MathExpr
  | Ellipsis
  | Parens     MathExpr
  | Sum        MathExpr MathExpr MathExpr               -- Σ(i=start..end) expr
  | Product    MathExpr MathExpr MathExpr               -- Π(i=start..end) expr
  | Integral   (Maybe (MathExpr,MathExpr)) MathExpr     -- ∫[a..b]? expr
  | Limit      MathExpr MathExpr                        -- lim₍var→val₎ expr
  | Derivative MathExpr MathExpr                        -- d/dx expr
  | Root       (Maybe MathExpr) MathExpr                -- ⁿ√expr
  | Binom      MathExpr MathExpr                        -- (n choose k)
  | Abs        MathExpr                                  -- |expr|
  | Vector     [MathExpr]                               -- ⟨v1,v2,…⟩
  | Matrix     [[MathExpr]]                             -- matriz de vetores
  | Func       String [MathExpr]                        -- f(arg1,arg2,…)
  | Piecewise  [(MathExpr,MathExpr)]                    -- { expr₁ if c₁; … }
  deriving (Show, Eq)

data Markers = MarkersMain String [MainSection]
               deriving (Show)