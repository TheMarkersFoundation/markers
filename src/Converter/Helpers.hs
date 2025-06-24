{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Converter.Helpers where

import Data.List
import Data.Maybe
import Ast.AbstractSyntaxTree
import qualified Data.Map.Strict as M

import Data.String.Interpolate.IsString (i)

-- Universal HTML Header
header :: String -> String -> String
header title lang = [i|
<!DOCTYPE html>
<html lang="#{lang}">
<head>
  <meta charset="UTF-8">
  <title>#{title}</title>
|]

closeHeader :: String
closeHeader = "</head>"

tex :: String
tex = [i|
  <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/mathjax@3/es5/output/chtml/fonts/tex.css">
|]

math :: String
math = [i|
  .math-block {
    font-family: 'MJXZERO', 'Latin Modern Math', serif;
    display: flex;
    font-size: 1.2em;
    line-height: 1;
    vertical-align: middle;
    align-items: center;
    justify-content: center;
    margin: 2em 0;
  }

  /* Fração empilhada estilo TeX */
  .math-block .fraction {
    display: inline-flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    margin: 0 0.5em;
    font-size: 1.1em;
  }
  .math-block .fraction .num {
    padding: 0 0.1em;
  }
  .math-block .fraction .den {
    padding: 0 0.1em;
    border-top: 0.07em solid currentColor;
    margin-top: 0.07em;
  }

  /* Expoentes (superscript) */
  .math-block sup {
    font-size: 0.7em;
    vertical-align: super;
    line-height: 1;
    margin-left: 0.05em;
  }

  /* Raiz quadrada */
  .math-block .radicand {
    display: inline-block;
    position: relative;
    padding-left: 0.5em;
  }
  .math-block .radicand::before {
    content: "\221A";
    position: absolute;
    left: 0;
    top: 0;
    font-size: 1em;
    line-height: 1;
  }
  .math-block .radicand {
    border-top: 0.07em solid currentColor;
    margin-left: -0.1em;
  }

  /* Operadores (mais uniforme) */
  .math-block .operator {
    margin: 0 0.2em;
    font-style: normal;
  }

  /* P para probabilidade mantém corpo reto, separador leva espaço */
  .math-block .probability {
    font-style: normal;
    margin-right: 0.3em;
  }
  .math-block .probability .sep {
    margin: 0 0.2em;
  }
|]

openStyle :: String
openStyle = [i|
  <style>
|]

closeStyle :: String
closeStyle = [i|
  </style>
|]

openScript :: String
openScript = [i|
  <script>
|]

closeScript :: String
closeScript = [i|
  </script>
|]

openBody :: String
openBody = [i|
<body>
|]

closeBody :: String
closeBody = [i|
</body>
|]

end :: String
end = [i|
</html>
|]
-- Resolve the Greek letters in a variable name to their Unicode equivalents.
greekMap :: M.Map String String
greekMap = M.fromList
  [ ("alpha",      "α"),   ("beta",       "β"),   ("gamma",  "γ")
  , ("delta",      "δ"),   ("epsilon",    "ε"),   ("zeta",   "ζ")
  , ("eta",        "η"),   ("theta",      "θ"),   ("iota",   "ι")
  , ("kappa",      "κ"),   ("lambda",     "λ"),   ("mu",     "μ")
  , ("nu",         "ν"),   ("xi",         "ξ"),   ("omicron","ο")
  , ("pi",         "π"),   ("rho",        "ρ"),   ("sigma",  "σ")
  , ("tau",        "τ"),   ("upsilon",    "υ"),   ("phi",    "φ")
  , ("chi",        "χ"),   ("psi",        "ψ"),   ("omega",  "ω")
  ]

-- Escape mathematical symbols to their HTML entities.
escapeMathSymbol :: String -> String
escapeMathSymbol = concatMap replaceChar
  where
    replaceChar '−' = "&minus;"
    replaceChar '·' = "&middot;"
    replaceChar '÷' = "&divide;"
    replaceChar '√' = "&radic;"
    replaceChar '→' = "&rarr;"
    replaceChar 'Σ' = "&Sigma;"
    replaceChar 'Π' = "&Pi;"
    replaceChar '∫' = "&int;"
    replaceChar '⎛' = "&#10216;"
    replaceChar '⎞' = "&#10217;"
    replaceChar '⎝' = "&#10218;"
    replaceChar '⎠' = "&#10219;"
    replaceChar '⟨' = "&lang;"
    replaceChar '⟩' = "&rang;"
    replaceChar c   = [c]

-- Escape HTML special characters in a string.
escapeHtml :: Char -> String
escapeHtml c = case c of
    '\n' -> "<br>"
    '<' -> "&lt;"
    '>' -> "&gt;"
    '&' -> "&amp;"
    '"' -> "&quot;"
    '\'' -> "&#39;"
    _ -> [c]

-- Split the list of MainSection into two parts: the summary section and the rest.
-- Used for ABNT and other formats that require a summary.
splitSections :: [MainSection] -> ([MainSection], [MainSection])
splitSections [] = ([], [])
splitSections xs =
  let (pre, post) = break isSummary xs
  in case post of
       []     -> (pre, [])
       (s:ss) -> (pre ++ [s], ss)

-- Check if a MainSection is a summary section.
isSummary :: MainSection -> Bool
isSummary (Summary _) = True
isSummary _           = False

-- Strip <p> tags from a string, removing both opening and closing tags.
stripPTags :: String -> String
stripPTags s =
  let withoutOpen  = fromMaybe s (stripPrefix "<p>" s)
      withoutClose = reverse $ fromMaybe (reverse withoutOpen) (stripPrefix (reverse "</p>") (reverse withoutOpen))
  in withoutClose

-- Configuration for ABNT style, including language, paper size, margins, font settings, etc.
data AbntConfig = AbntConfig
  { lang                  :: String
  , paperSize             :: String
  , marginTop             :: String
  , marginBottom          :: String
  , marginLeft            :: String
  , marginRight           :: String
  , fontFamily            :: String
  , fontSize              :: String
  , chapterSize           :: String
  , textSize              :: String
  , lineHeight            :: String
  , titleAlign            :: String
  , titleSize             :: String
  , titleBold             :: Bool
  , imageSize             :: String
  , boldSectionTitles     :: Bool
  , boldWholeNumber       :: Bool
  , figureAlign           :: String
  , figureSize            :: String
  , figureBold            :: Bool
  , figureNumberBold      :: Bool
  , referencesAlphabetic  :: Bool
  }

-- Default configuration for ABNT style, with common settings.
defaultAbntConfig :: AbntConfig
defaultAbntConfig = AbntConfig
  { lang = "pt-BR"
  , paperSize = "A4"
  , marginTop = "3"
  , marginBottom = "2"
  , marginLeft = "2"
  , marginRight = "3"
  , fontFamily = "'Times New Roman', Times, serif"
  , fontSize = "12"
  , chapterSize = "14"
  , textSize = "12"
  , lineHeight = "1.5"
  , titleAlign = "center"
  , titleSize = "14"
  , titleBold = False
  , imageSize = "100"
  , boldSectionTitles = False
  , boldWholeNumber = False
  , figureAlign = "center"
  , figureSize = "14"
  , figureBold = False
  , figureNumberBold = False
  , referencesAlphabetic = True
  }

-- Apply a list of preferences to the default ABNT configuration.
applyPreferences :: [Preferences] -> AbntConfig
applyPreferences prefs = foldr apply defaultAbntConfig prefs
  where
    apply (Language l) cfg = cfg { lang = l }

    apply (Page elems) cfg = foldr applyPage cfg elems
      where
        applyPage (PageSize s) c = c { paperSize = s }
        applyPage (PageNumberSize _) c = c
        applyPage (PageMargin margins) c = foldr applyMargin c margins

        applyMargin (MarginTop s) c = c { marginTop = s }
        applyMargin (MarginBottom s) c = c { marginBottom = s }
        applyMargin (MarginLeft s) c = c { marginLeft = s }
        applyMargin (MarginRight s) c = c { marginRight = s }

    apply (Content cs) cfg = foldr applyContent cfg cs
      where
        applyContent (FontArial True) c = c { fontFamily = "Arial, sans-serif" }
        applyContent (FontTimes True) c = c { fontFamily = "'Times New Roman', Times, serif" }
        applyContent (FontOther f) c = c { fontFamily = f }
        applyContent (TitleSize s) c = c { fontSize = s }
        applyContent (ChapSize s) c = c { chapterSize = s }
        applyContent (TextSize s) c = c { textSize = s }
        applyContent (LineHeight s) c = c { lineHeight = s }
        applyContent (ImageSize s) c = c { imageSize = s }
        applyContent (BoldSectionTitles True) c = c { boldSectionTitles = True }

    apply (SummaryPrefs ss) cfg = foldr applySummary cfg ss
      where
        applySummary (SummaryTitleAlignCenter True) c = c { titleAlign = "center" }
        applySummary (SummaryTitleAlignLeft True)   c = c { titleAlign = "left" }
        applySummary (SummaryTitleSize s)           c = c { titleSize = s }
        applySummary (SummaryTitleBold True)        c = c { titleBold = True }
        applySummary (SummaryBoldWholeNumber True)  c = c { boldWholeNumber = True }
        applySummary _                              c = c

    apply (FiguresPrefs fs) cfg = foldr applyFigures cfg fs
      where
        applyFigures (FiguresTitleAlignCenter True) c = c { figureAlign = "center" }
        applyFigures (FiguresTitleAlignLeft True)   c = c { figureAlign = "left" }
        applyFigures (FiguresTitleSize s)           c = c { figureSize = s }
        applyFigures (FiguresTitleBold True)        c = c { figureBold = True }
        applyFigures (FiguresBoldNumber True)       c = c { figureNumberBold = True }
        applyFigures _                              c = c

    apply (ReferencesPrefs rs) cfg = foldr applyRef cfg rs
      where
        applyRef (Alphabetic True) c = c { referencesAlphabetic = True }
