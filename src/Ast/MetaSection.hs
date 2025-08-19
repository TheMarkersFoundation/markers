module Ast.MetaSection where

{- 

    This is the MetaSection Abstract Syntax Tree.
    It handles all of the metadata of the document.

    Metadata does show in academic formats, like
    ABNT. So it's always recommended to put
    them in. Although not obligatory.

    It exists under the (meta) and (/meta) tags.
    
-}

data MetaSection = Author String             -- (author) and (/author)
                 | Institution String        -- (institution) and (/institution)
                 | Subtitle String           -- (subtitle) and (/subtitle)
                 | Location String           -- (location) and (/location)
                 | Year String               -- (year) and (/year)
                 | Description String        -- (description) and (/description)
                 | Mentor String             -- (mentor) and (/mentor)
                 deriving (Show)