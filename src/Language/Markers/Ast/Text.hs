module Language.Markers.Ast.Text where
import Language.Markers.Ast.Types

{-

    Writing represents formatted text with various styles such as bold, italic,
    underline, strikethrough, monospaced, colored, and highlighted.

    As such, these styles can be nested within each other to create complex
    formatting, except for the Plain constructor which represents unformatted
    text, and it's therefore an atomic unit.

-}

{-

    RefType identifies which ABNT reference style a citation follows. Each style
    expects a different, fixed set of pipe-separated fields (see the parser and
    the converters for the field order and rendering of each type).

      WebRef     documento online   url|author|title|year|access
      BookRef    livro              author|title|edition|city|publisher|year
      ArticleRef artigo de periodico author|title|journal|volume|number|pages|year
      ChapterRef capitulo de livro  author|chapterTitle|bookAuthor|bookTitle|city|publisher|year|pages
      ThesisRef  tese/dissertacao   author|title|workType|institution|city|year

-}

data RefType = WebRef
             | BookRef
             | ArticleRef
             | ChapterRef
             | ThesisRef
             deriving (Show, Eq, Ord)

data Writing = Plain Atom
             | Bold [Writing]
             | Italic [Writing]
             | Underline [Writing]
             | Strikethrough [Writing]
             | Monospaced [Writing]
             | Link [Writing] Atom
             | Reference [Writing] RefType [Atom]
             | Footnote [Writing]
             | MathInline Atom
             | Raw Atom
             | Colored [Writing] Color
             | Highlighted [Writing] Color
             deriving (Show, Eq)

{-

    MathInline holds the raw Euler source of an inline !math(...) span. A
    resolution pass (Language.Markers.Math) expands its definitions and renders
    it, replacing it with a Raw node.

    Raw holds verbatim HTML produced by such a pass; renderers emit it as-is.

-}
