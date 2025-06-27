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
                 | Mentor String
                 deriving (Show)

data Abbr = Abbr String String
  deriving (Show, Eq)

data MainSection =  
                Empty
                | Centered [MainSection]
                | RightContent [MainSection]
                | Highlighted String [MainSection]
                | NumberedList [[MainSection]]
                | BulletList [[MainSection]]
                | LetteredList [[MainSection]]
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
                | Abntchapter String String [MainSection]
                | Commentary String
                | LineBreak
                | Tab
                | Separator
                | References
                | ReferencesPaged String
                | Figurelist
                | MathList
                | MathBlock [MathExpr]
                | MathBlockWithPage String [MathExpr]
                | Summary String
                | Thanks [MainSection]
                | Abstract String [MainSection]
                | Abbreviations String [Abbr]
                deriving (Show)

data MathExpr = 
                Number String
                | Add MathExpr MathExpr
                | Sub MathExpr MathExpr
                | Mul MathExpr MathExpr
                | ImplicitMul MathExpr MathExpr
                | Div MathExpr MathExpr
                | PowerOf    MathExpr MathExpr
                | SquareRoot MathExpr
                | Fraction   MathExpr MathExpr
                | Var String
                | Eq         MathExpr MathExpr
                | Arrow      MathExpr MathExpr
                | Neg MathExpr
                | Probability MathExpr MathExpr
                | Ellipsis
                | Parens     MathExpr
                | Sum        MathExpr MathExpr MathExpr
                | Product    MathExpr MathExpr MathExpr
                | Integral   (Maybe (MathExpr,MathExpr)) MathExpr
                | Limit      MathExpr MathExpr
                | Derivative MathExpr MathExpr
                | Root       (Maybe MathExpr) MathExpr
                | Binom      MathExpr MathExpr
                | Abs        MathExpr
                | Vector     [MathExpr]
                | Matrix     [[MathExpr]]
                | Func       String [MathExpr]
                | Piecewise  [(MathExpr,MathExpr)]
                deriving (Show, Eq)

data Preferences = Language String
                 | Page [PageElement]
                 | Content [Content]
                 | SummaryPrefs [SummaryPreferences]
                 | FiguresPrefs [FigureListPreferences]
                 | ReferencesPrefs [ReferencesPreferences]
                deriving (Show)

data PageElement = PageSize String
                 | PageNumberSize String
                 | PageMargin [Margin]
                deriving (Show)

data Margin = MarginTop String | MarginBottom String | MarginLeft String | MarginRight String
    deriving (Show)

data Content = FontArial Bool 
             | FontTimes Bool
             | FontOther String
             | TitleSize String
             | ChapSize String 
             | TextSize String 
             | LineHeight String
             | ImageSize String
             | BoldSectionTitles Bool
            deriving (Show)

data SummaryPreferences = SummaryTitleAlignCenter Bool 
                        | SummaryTitleAlignLeft Bool
                        | SummaryTitleSize String 
                        | SummaryTitleBold Bool
                        | SummaryChapBold Bool 
                        | SummaryBoldNumber Bool 
                        | SummaryBoldWholeNumber Bool
                      deriving (Show)

data FigureListPreferences = FiguresTitleAlignCenter Bool
                           | FiguresTitleAlignLeft Bool
                           | FiguresTitleSize String 
                           | FiguresTitleBold Bool
                           | FiguresChapBold Bool 
                           | FiguresBoldNumber Bool
    deriving (Show)

data ReferencesPreferences = Alphabetic Bool
    deriving (Show)


data Markers = MarkersMain String [Preferences] [MainSection]
               deriving (Show)