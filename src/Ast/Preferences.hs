module Ast.Preferences where

{- 

    This is the Preferences Abstract Syntax Tree.
    It handles all of the preferences and customization
    Tags for Markers. This is a work in progress.

    TODO: Make this more legible and better.

-}

{- 

   Preferences handles the (preferences) tag.
   It has parameter tags exclusive to it as per
   easier and more organized customization.

    It's usage is:
    (preferences)
        (-- Parameter Tags --)
    (/preferences)

-}

data Preferences = Language String                          -- (language) and (/language)
                 | Page [PageElement]                       -- (page) and (/page)
                 | Behaviour HelpieBehaviour                -- See HelpieBehaviour
                 | Content [Content]                        -- (content) and (/content)
                 | SummaryPrefs [SummaryPreferences]        -- (summary) and (/summary)
                 | FiguresPrefs [FigureListPreferences]     -- (figures) and (/figures)
                 | ReferencesPrefs [ReferencesPreferences]  -- (references) and (/references)
                deriving (Show)

-- Parameter tags for the Page [(page) and (/page)] tag.
data PageElement = PageSize String                          -- (size) and (/size)
                 | PageNumberSize String                    -- (page-number-size) and (/page-number-size)
                 | PageMargin [Margin]                      -- (margin) and (/margin)
                 | PageBackgroundColor String               -- (background-color) and (/background-color)
                 | PageTextColor String                     -- (text-color) and (/text-color)
                deriving (Show)

-- Parameter tags for the PageMargin [(margin) and (/margin)] tag.
data Margin = MarginTop String | MarginBottom String | MarginLeft String | MarginRight String
    deriving (Show)

{- 

   HelpieBehaviour determines the behaviour
   of Helpie tags. Read more about helpies
   in the Ast.Helpy module. 

-}

data HelpieBehaviour = Default                              -- (helpie-default)
                       | Abnt                               -- (helpie-abnt)
                       deriving (Show)

{- 

   Content - or the (content) tag is used to
   configure the content of the document. This tag
   is used to add information about the content, such
   as the font size, font family, and other relevant information.
   
   It has parameter tags used to configure said preferences.

   This is a currently BETA attempt at full-on Markers customization,
   and currently only works for ABNT.

-}

-- Parameter tags for the Page [(content) and (/content)] tag.
data Content = FontArial Bool                               -- (font-arial)
             | FontTimes Bool                               -- (font-times)
             | FontOther String                             -- (font-other)'Name of Font', 'font'(/font-other)
             | TitleSize String                             -- (title-size)12(/title-size)
             | ChapSize String                              -- (chap-size)12(/chap-size)
             | TextSize String                              -- (text-size)12(/text-size)
             | LineHeight String                            -- (line-height)1.2(/line-height)
             | ImageSize String                             -- (image-size)80(/image-size)
             | BoldSectionTitles Bool                       -- (bold-section-titles)
            deriving (Show)


{- 

   SummaryPreferences - or the (summary) tag is used
   to configure the summary of the document.
   
   This tag is used to add or change information about the summary.
   
   This is a currently BETA attempt at full-on Markers customization,
   and currently only works for ABNT.

-}

-- Parameter tags for the Page [(summary) and (/summary)] tag.
data SummaryPreferences = SummaryTitleAlignCenter Bool       -- (title-align-center)
                        | SummaryTitleAlignLeft Bool         -- (title-align-left)
                        | SummaryTitleSize String            -- (title-size) and (/title-size)
                        | SummaryTitleBold Bool              -- (title-bold)
                        | SummaryChapBold Bool               -- (chap-bold)
                        | SummaryBoldNumber Bool             -- (bold-number)
                        | SummaryBoldWholeNumber Bool        -- (bold-whole-number)
                      deriving (Show)

{- 

   SummaryPreferences - or the (summary) tag is used
   to configure the summary of the document.
   
   This tag is used to add or change information about the summary.
   
   This is a currently BETA attempt at full-on Markers customization,
   and currently only works for ABNT.

-}

-- Parameter tags for the Page [(figures) and (/figures)] tag.
data FigureListPreferences = FiguresTitleAlignCenter Bool     -- (title-align-center)
                           | FiguresTitleAlignLeft Bool       -- (title-align-left)
                           | FiguresTitleSize String          -- (title-size) and (/title-size)
                           | FiguresTitleBold Bool            -- (title-bold)
                           | FiguresChapBold Bool             -- (chap-bold)
                           | FiguresBoldNumber Bool           -- (bold-number
    deriving (Show)

{- 

   ReferencesPreferences - or the (references) tag is used
   to configure the behaviour of references on the document.
   
   This is a currently BETA attempt at full-on Markers customization,
   and currently only works for ABNT.

-}

data ReferencesPreferences = Alphabetic Bool                  -- (order-alphabetic)
    deriving (Show)
