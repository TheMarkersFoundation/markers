module Ast.TextTag where

{- 

    This is the TextTag Abstract Syntax Tree.
    It handles (and documents) all of the possible
    Text tags. Use this file for reference and additions.

-}

data TextTag = Plain String             -- Plain Text
            | Crossed [TextTag]         -- (c) and (/c)
            | Underlined [TextTag]      -- (u) and (/u)
            | Bold [TextTag]            -- (b) and (/b)
            | Italic [TextTag]          -- (i) and (/i)
            | CodeInline String         -- (k) and (/k)
            | Small [TextTag]           -- (sm) and (/sm)
            | Top [TextTag]             -- (tp) and (/tp)
            | Color String [TextTag]    -- (color | hex) and (/color)
            deriving (Show)