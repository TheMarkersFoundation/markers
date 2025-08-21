{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Helpers.General where

import Ast.AbstractSyntaxTree
import Helpers.Helpers

import Data.String.Interpolate.IsString (i)

-- Auxiliary curried function that applies
-- concatMap to the converted text.
treatText :: [TextTag] -> String
treatText = concatMap (convertText False)

-- Universal Text Tag Conversion
convertText :: Bool -> TextTag -> String
convertText True  (Plain text)      = [i|<p class="indent" id="contentText">#{text}</p>|]
convertText False (Plain text)      = text
convertText _ (Bold text)           = [i|<strong>#{treatText text}</strong>|]
convertText _ (Italic text)         = [i|<em>#{treatText text}</em>|]
convertText _ (Underlined text)     = [i|<span style="text-decoration:underline">#{treatText text}</span>|]
convertText _ (Crossed text)        = [i|<span style="text-decoration:line-through">#{treatText text}</span>|]
convertText _ (CodeInline text)     = [i|<code id="code_text">#{text}</code>|]
convertText _ (Small text)          = [i|<small>#{treatText text}</small>|]
convertText _ (Top text)            = [i|<sup>#{treatText text}</sup>|]
convertText _ (Color col text)      = [i|<span style="color:"#{col}">#{treatText text}</span>|]

-- Universal Helpies Conversion
convertHelpie :: Helpies -> Config -> String
convertHelpie LineBreak cfg =
  case behave cfg of
    Default -> [i|<br>|]
    Abnt    -> [i||]

convertHelpie Separator cfg =
  case behave cfg of
    Default -> [i|<hr>|]
    Abnt    -> [i|<div class="separator" style="page-break-before: always;"></div>|]

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


body :: String -> String -> String -> String -> String -> String -> String -> String
body bgcolor textcolor font titleSize chapterTitleSize textSize lineHeight = [i| 
    body {
        font-family: #{font};
        font-size: #{textSize}pt;
        line-height: #{lineHeight};
        text-align: justify;
        background-color: #{bgcolor};
        color: #{textcolor};
        margin: 0;
        padding: 0;
    }

    h1 { font-size: #{titleSize}pt; }
    h2, h3, h4, h5, h6 {
      text-align: left;
      margin: 1.5em 0 0.5em 0;
      font-size: #{chapterTitleSize}pt;
    }
|]

container :: String -> String
container bgcolor = [i|
    .markers-content-container {
      max-width: 800px;
      margin: 2em auto;
      padding: 2em;
      background-color: #{bgcolor};
      border-radius: 8px;
    }
|]