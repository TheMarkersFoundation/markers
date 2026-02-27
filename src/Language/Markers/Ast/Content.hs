module Language.Markers.Ast.Content where

import Language.Markers.Ast.Text (Writing)
import Language.Markers.Ast.Types (Title, File)

{-

    The Content data structure represents the different types of content that can be
    found within a Markers document. It includes paragraphs, which are made up of
    formatted text (Writing), and chapters, which can contain nested content.

-}


data Content = Paragraph [Writing]
             | Url File [Writing]
             | Figure File [Writing] [Writing]
             | Table [Title] [[Title]]
             | Footer Int [Writing]
             | Chapter Int Title [Content]
             | ArrowList Int Title [Content]
             | CodeBlock String
             | BulletList Int [Content]
             | Break
             | Summary
             | FigureList
             | References
             deriving (Show, Eq)
