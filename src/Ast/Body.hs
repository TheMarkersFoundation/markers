module Ast.Body where

import Ast.Math
import Ast.MetaSection
import Ast.Preferences
import Ast.TextTag
import Ast.Helpy

{- 

    This is the MainSection Abstract Syntax Tree.
    It handles (and documents) all of the possible
    body tags. Most of this is recursive in nature.

    Every time a new tag is created on Markers, it
    should ALWAYS be inserted here, unless it is a
    (meta), (preferences) or any other tag that has
    behaviour that do not affect the content of the
    document directly.

    TODO: Make this more legible.
-}

data MainSection =  
                Empty                                                       -- Leaf.
                | Paragraph Paragram                                        -- Any text or Text Tag.
                | Chap String [MainSection]                                 -- (chap | Title)
                | PagedChapter String String [MainSection]                  -- (chap | 0 | Title)
                | List String [MainSection]                                 -- (>> | Title) and (/>>)
                | Code Paragram                                             -- (code) and (/code)
                | CodeNamed String [MainSection]                            -- (code | haskell) and (/code)
                | NumberedList [[MainSection]]                              -- (nl) and (/nl)
                | BulletList [[MainSection]]                                -- (bl) and (/bl)
                | LetteredList [[MainSection]]                              -- (ll) and (/ll)
                | Helpy Helpies                                             -- (br) and (hr). See Helpy module.
                | Centered [MainSection]                                    -- (align-center) and (/align-center)
                | RightContent [MainSection]                                -- (align-right) and (/align-right)
                | Ref String String String String String [MainSection]      -- (ref | url | name-of-the-author | year | date-of-access) and (/ref)
                | Meta [MetaSection]                                        -- (meta) and (/meta). See MetaSection module.
                | Link String Paragram                                      -- (link | https://markers.mirvox.xyz ) and (/link).
                | Image String String Paragram                              -- (localimg | cat.jpg)lolcat.(/localimg)
                | ImageUrl String Paragram                                  -- (img | https://markers.mirvox.xyz/static/logo3.png)lolcat.(/img)
                | ImagePage String String String Paragram                   -- (localimg | 0 | cat.jpg)lolcat.(/localimg)
                | ImageUrlPage String String Paragram                       -- (img | 0 | https://markers.mirvox.xyz/static/logo3.png)lolcat.(/img)
                | Video String Paragram                                     -- (video | planet.mp4) Video. (/video)
                | Audio String Paragram                                     -- (audio | musica.mp3) Audio. (/audio)
                | Table [String] [[String]]                                 -- (table) (x | y | z) (/table)
                | Quote String Paragram                                     -- (quote | author) and (/quote)
                | Commentary String                                         -- (-- and --)
                | MathBlock [MathExpr]                                      -- (math) and (/math)
                | MathBlockWithPage String [MathExpr]                       -- (math | 0) and (/math)
                | Thanks [MainSection]                                      -- (thanks) and (/thanks)
                | Abstract String [MainSection]                             -- (abstract | Title) and (/abstract)
                | Abbreviations String [Abbr]                               -- (abbreviations | Title) and (/abbreviations)
                | References                                                -- (references)
                | ReferencesPaged String                                    -- (references | 0)
                | Figurelist                                                -- (figurelist)
                | MathList                                                  -- (mathlist)
                | Summary String                                            -- (summary)
                deriving (Show)

{- 

  An Abbr is an abbreviation from the (abbreviations) tag.

  (abbreviations | Title)
  ABBR - Abbreviation
  (/abbreviations)

-}

data Abbr = Abbr String String
  deriving (Show, Eq)

{- 

  A Paragram is an abstraction for any Text Tag.
  See TextTag module.

-}

type Paragram = [TextTag]

{- 

  The Markers Document Algebraic Data Type.
  Handles the Title, Preferences and the Body section.
  
-}

data Markers = MarkersMain String [Preferences] [MainSection]
               deriving (Show)