module Language.Euler.Env (Env, emptyEnv) where

import qualified Data.Map.Strict as M
import Language.Euler.Ast.Expr (Expr)

{-

    An Env maps a defined name to the expression it stands for. It is built once
    from every definition in a document and used to expand references (so that,
    e.g., a name defined as 'pi*r^2' renders as π·r² wherever it appears).

-}

type Env = M.Map String Expr

emptyEnv :: Env
emptyEnv = M.empty
