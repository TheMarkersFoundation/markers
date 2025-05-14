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

data MainSection =  
                Empty
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
                | Summary String
                | Abntchapter String String [MainSection]
                | Commentary String
                | LineBreak
                | Tab
                | Separator
                | References
                | Figurelist
                deriving (Show)

data Markers = MarkersMain String [MainSection]
               deriving (Show)