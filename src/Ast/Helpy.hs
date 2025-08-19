module Ast.Helpy where

{- 

   Helpies are HTML inspired tags that
   have different behaviour depending
   on the current type of document format.

-}

data Helpies = LineBreak        -- \n or (br)
             | Separator        -- (hr)
    deriving (Show)