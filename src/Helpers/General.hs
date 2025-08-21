{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Helpers.General where

import Ast.AbstractSyntaxTree
import Helpers.Helpers
import Data.FileEmbed (embedStringFile)
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

codeStyle :: String -> String -> String
codeStyle textColor bgColor = [i|
  <style>
    pre code.hljs {
      display: block;
      overflow-x: auto;
      padding: 1em;
      border-radius: 0.5em;
      background: #{bgColor};
      color: #{textColor};
    }

    code.hljs {
      padding: 3px 5px;
      border-radius: 0.25em;
    }

    .hljs-comment,
    .hljs-quote {
      color: #6a737d;
      font-style: italic;
    }

    .hljs-keyword,
    .hljs-selector-tag,
    .hljs-literal,
    .hljs-section,
    .hljs-link {
      color: #d73a49;
    }

    .hljs-function .hljs-title,
    .hljs-title.class_,
    .hljs-title.function_ {
      color: #6f42c1;
    }

    .hljs-attr,
    .hljs-attribute,
    .hljs-name,
    .hljs-tag {
      color: #22863a;
    }

    .hljs-string,
    .hljs-bullet,
    .hljs-symbol,
    .hljs-addition {
      color: #032f62;
    }

    .hljs-number,
    .hljs-meta,
    .hljs-built_in,
    .hljs-type {
      color: #005cc5;
    }

    .hljs-variable,
    .hljs-template-variable {
      color: #e36209;
    }

    .hljs-deletion {
      color: #b31d28;
    }

    .hljs-strong {
      font-weight: bold;
    }

    .hljs-emphasis {
      font-style: italic;
    }
  </style>
|]

codeHighlight :: String
codeHighlight = $(embedStringFile "src/Helpers/External/highlight.js")