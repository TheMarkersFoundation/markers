module Language.Euler.Ast.Block where

import Language.Euler.Ast.Expr (Expr)

{-

    A Euler block (the content of a single !math(...) tag) is a list of
    statements. A statement is either a named definition or a standalone
    expression / equation.

-}

data Stmt
  = Define String Expr  -- ^ 'def name := expr' or simply 'name := expr'
  | Express Expr        -- ^ a standalone expression or equation
  deriving (Show, Eq)

type Block = [Stmt]
