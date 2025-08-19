module Ast.Math where

data MathExpr = 
                Number String
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
                | Sum        MathExpr MathExpr MathExpr
                | Product    MathExpr MathExpr MathExpr
                | Integral   (Maybe (MathExpr,MathExpr)) MathExpr
                | Limit      MathExpr MathExpr
                | Derivative MathExpr MathExpr
                | Root       (Maybe MathExpr) MathExpr
                | Binom      MathExpr MathExpr
                | Abs        MathExpr
                | Vector     [MathExpr]
                | Matrix     [[MathExpr]]
                | Func       String [MathExpr]
                | Piecewise  [(MathExpr,MathExpr)]
                deriving (Show, Eq)