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

data AbntSection = Author String
                 | Institution String
                 | Subtitle String
                 | Location String
                 | Year String
                 deriving (Show)

data MainSection =  
                Empty
                | Paragraph TextTag
                | Abnt [AbntSection]
                | List String [MainSection]
                | Chap String [MainSection]
                | Ref String String String String String [MainSection]
                | Link String [MainSection]
                | Image String String [MainSection]
                | ImageUrl String [MainSection]
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
                deriving (Show)

data Markers = MarkersMain String [MainSection]
               deriving (Show)