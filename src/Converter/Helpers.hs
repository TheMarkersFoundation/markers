module Converter.Helpers where

import Data.List
import Data.Maybe
import Ast.AbstractSyntaxTree
import qualified Data.Map.Strict as M


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