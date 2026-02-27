module Language.Markers.Ast.Text where
import Language.Markers.Ast.Types

{-

    Writing represents formatted text with various styles such as bold, italic,
    underline, strikethrough, monospaced, colored, and highlighted.

    As such, these styles can be nested within each other to create complex
    formatting, except for the Plain constructor which represents unformatted
    text, and it's therefore an atomic unit.

-}

data Writing = Plain Atom
             | Bold [Writing]
             | Italic [Writing]
             | Underline [Writing]
             | Strikethrough [Writing]
             | Monospaced [Writing]
             | Link [Writing] Atom
             | Reference [Writing] Atom Atom Atom Atom Atom
             | Colored [Writing] Color
             | Highlighted [Writing] Color
             deriving (Show, Eq)
