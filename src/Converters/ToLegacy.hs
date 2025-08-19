{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Converters.ToLegacy where
import Data.List
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Data.List.Split (splitOn)
import Ast.AbstractSyntaxTree
import Data.Char (isAlpha)
import Data.Maybe (fromMaybe)

import Helpers.General
import Helpers.Helpers
import Helpers.Math (renderMath)

import Data.String.Interpolate.IsString (i)

toStyledHtml :: Markers -> String
toStyledHtml (MarkersMain title prefs sections) =
    "<!DOCTYPE html>\
    \<html lang=\"en\">\
    \<head>\
    \  <script src=\"https://unpkg.com/htmx.org@2.0.4\"></script>\
    \  <meta charset=\"UTF-8\">\
    \  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\
    \  <link href=\"https://fonts.googleapis.com/css2?family=Fira+Code&display=swap\" rel=\"stylesheet\">\
    \  <title>" <> title <> "</title>\
    \  <style>\
    \    body {\
    \      margin: 0;\
    \      padding: 0;\
    \      line-height: 1.6;\
    \      color: #333;\
    \      text-align: justify;\
    \      text-justify: inter-word;\
    \    }\
    \    .container {\
    \      max-width: 800px;\
    \      margin: 2em auto;\
    \      padding: 2em;\
    \      background-color: #fff;\
    \      border-radius: 8px;\
    \    }\
    \    .container h1, .container h2, .container h3 {\
    \      text-align: left;\
    \      margin-top: 1.2em;\
    \      margin-bottom: 0.8em;\
    \    }\
    \    .container p {\
    \      line-height: 1.6;\
    \      text-align: justify;\
    \      margin: 1em 0;\
    \    }\
    \    details {\
    \      margin: 10px 0;\
    \    }\
    \    summary {\
    \      cursor: pointer;\
    \      font-weight: bold;\
    \      padding: 5px 0;\
    \    }\
    \    img {\
    \      max-width: 100%;\
    \      height: auto;\
    \      display: block;\
    \      margin: 1em auto;\
    \    }\
    \    pre {\
    \      background: #f6f8fa;\
    \      color: #24292e;\
    \      padding: 1em;\
    \      overflow-x: auto;\
    \      border-radius: 5px;\
    \      margin: 1em 0;\
    \      font-family: 'Fira Code', Consolas, Monaco, 'Andale Mono', 'Ubuntu Mono', monospace;\
    \      font-size: 0.9em;\
    \      font-feature-settings: \"liga\" on, \"calt\" on;\
    \    }\
    \    .modern-quote {    \
    \        font-style: italic;    \
    \        color: #333;   \
    \        border-left: 4px solid #333;    \
    \        padding: 10px 20px;    \
    \        margin: 10px 0;    \
    \        background: #f9f9f9;   \
    \    }  \
    \   \
    \    .modern-quote footer { \
    \        margin-top: 5px;   \   
    \        font-weight: bold; \
    \        text-align: right; \
    \        font-style: normal;    \
    \    }  \
    \    \
    \    code {\
    \      background: #eaeaea;\
    \      padding: 0.2em 0.4em;\
    \      border-radius: 4px;\
    \      font-size: 0.9em;\
    \      font-family: 'Fira Code', Consolas, Monaco, 'Andale Mono', 'Ubuntu Mono', monospace;\
    \      font-feature-settings: \"liga\" on, \"calt\" on;\
    \    }\
    \.operator { color: red; }\
    \.keyword { color: yellow; }\
    \.equal { color: purple; }\
    \.special-keyword { color: pink; }\
    \.adt-keyword { color: blue; }\
    \    .string {\
    \      color: #032f62;\
    \    }\
    \    .comment {\
    \      color: #6a737d;\
    \      font-style: italic;\
    \    }\
    \    /* Added table styles */\
    \    table {\
    \      width: 100%;\
    \      border-collapse: collapse;\
    \      margin: 1em 0;\
    \    }\
    \    td {\
    \      border: 1px solid #ddd;\
    \      padding: 8px;\
    \      text-align: center;\
    \    }\
    \    th {\
    \      background-color: #f2f2f2;\
    \      font-weight: bold;\
    \      padding: 8px;\
    \      border: 1px solid #ddd;\
    \      text-align: center;\
    \    }\
    \  </style>\
    \</head>\
    \<body>\
    \<div class=\"container\">\
    \<h1>" <> title <> "</h1>"
    <> Prelude.foldr (\x acc -> helper x <> acc) "" sections <>
    "</div>\
    \<script>\
    \  document.addEventListener('DOMContentLoaded', () => {\
    \    const allDetails = document.querySelectorAll('details');\
    \    allDetails.forEach(det => {\
    \      det.addEventListener('toggle', () => {});\
    \    });\
    \  });\
    \  document.querySelectorAll('.chapter h2').forEach(h2 => {\
    \    const level = h2.closest('.chapter').parentElement?.closest('.chapter') ? 2 : 1;\
    \    h2.style.fontSize = level === 2 ? '1.2em' : '1.5em';\
    \  });\
    \  const summaryDiv = document.querySelector('.summary');\
    \  const chapters = document.querySelectorAll('.chapter h2');\
    \  const ul = document.createElement('ul');\
    \  chapters.forEach(chapter => {\
    \    const li = document.createElement('li');\
    \    li.textContent = chapter.textContent;\
    \    ul.appendChild(li);\
    \  });\
    \  try { summaryDiv.appendChild(ul); } catch {}\
    \</script>\
    \</body>\
    \</html>"
    where
        helper :: MainSection -> String
        helper (Paragraph tags) = treatText tags
        helper (Helpy x)        = convertHelpie x (applyAbntPreferences prefs)

        helper (Commentary content)                 = "<!-- " <> content <> " -->"  

        helper (NumberedList items) =
          "<ol>"
          <> concatMap (\secs ->
              "<li>"
              <> concatMap helper secs
              <> "</li>"
            ) items
          <> "</ol>"

        helper (Ref url author title year access content)
            = "<a href=\"" <> url <> "\">" <> title <> "</a>"

        helper (List title content)
            = "\n<details><summary>\n" <> title <> "\n</summary>\n"
            <> "<div>" <> Prelude.foldr (\x acc -> helper x <> acc) "" content <> "</div>"
            <> "</details>\n"

        helper (Chap title content)
            = "\n<div class=\"chapter\"><h2>" <> title <> "</h2>\n"
            <> Prelude.foldr (\x acc -> helper x <> acc) "" content
            <> "</div>\n"

        helper (Centered content)
            = "\n<div style=\"text-align: center;\">" <> Prelude.foldr (\x acc -> helper x <> acc) "" content <> "</div>\n"

        helper (RightContent content)
            = "\n<div style=\"text-align: right;\">" <> Prelude.foldr (\x acc -> helper x <> acc) "" content <> "</div>\n"

        helper (Figurelist) = "<div class=\"figurelist\"><h3>LISTA DE FIGURAS</h3></div>"

        helper (PagedChapter _ title content)
            = "\n<div class=\"chapter\"><h2>" <> title <> "</h2>\n"
            <> Prelude.foldr (\x acc -> helper x <> acc) "" content
            <> "</div>\n"

        helper (Quote author content) =
            "<blockquote class=\"modern-quote\">"
            <> "<p>" <> treatText content <> "</p>"
            <> "<footer><cite>" <> author <> "</cite></footer>"
            <> "</blockquote>"

        helper (Link url content)
            = "\n<a href=\"" <> url <> "\">"
            <> treatText content
            <> "</a>\n"

        helper (Image base64String mimeType content)
            = "\n<img src=\"data:image/" <> mimeType <> ";base64," <> base64String <> "\" alt=\""
            <> treatText content
            <> "\">\n"

        helper (ImageUrl url content)
            = "\n<img src=\"" <> url <> "\" alt=\""
            <> treatText content
            <> "\">\n"

        helper (ImagePage _ base64String mimeType content)
            = "\n<img src=\"data:image/" <> mimeType <> ";base64," <> base64String <> "\" alt=\""
            <> treatText content
            <> "\">\n"

        helper (ImageUrlPage _ url content)
            = "\n<img src=\"" <> url <> "\" alt=\""
            <> treatText content
            <> "\">\n"

        helper (Video url content)
            = "<center><video src=\"" <> url <> "\" style=\"width: 60%\" controls>\n"
            <> treatText content
            <> "</video></center>\n"

        helper (Audio url content)
            = "<center><audio src=\"" <> url <> "\" controls>\n"
            <> treatText content
            <> "</audio></center>\n"

        helper (Code content)
            = "<pre>\n"
            <>  treatText content
            <> "</pre>\n"

        helper (Table headers rows)
            = "<table>\n"
            <> "<thead>\n"
            <> "<tr>\n"
            <> Prelude.foldr (\x acc -> "<th>" <> x <> "</th>" <> acc) "" headers
            <> "</tr>\n"
            <> "</thead>\n"
            <> "<tbody>\n"
            <> Prelude.foldr (\x acc -> "<tr>\n" <> Prelude.foldr (\y xcc -> "<td>" <> y <> "</td>" <> xcc) "" x <> "</tr>\n" <> acc) "" rows
            <> "</tbody>\n"
            <> "</table>\n"

        helper (NumberedList items) = let liItems = concatMap (\secs -> [i|<li>#{concatMap helper secs}</li>|]) items
          in [i|
            <ol>
                #{liItems}
            </ol>
          |]

        helper (BulletList items) = let liItems = concatMap (\secs -> [i|<li>#{concatMap helper secs}</li>|]) items
          in [i|
          <ul>
            #{liItems}
          </ul>
          |]

        helper (LetteredList items) = let liItems = concatMap (\secs -> [i|<li>#{concatMap helper secs}</li>|]) items
          in [i|
          <ol type="a">
            #{liItems}
          </ol>
          |]

        helper (Meta content) = case content of 
            [Institution c] -> "<div style=\"display: none;\" class=\"institution\">" <> Prelude.concatMap escapeHtml c <> "</div>"
            [Author c]      -> "<div style=\"display: none;\" class=\"author\">" <> c <> "</div>"
            [Subtitle c]    -> "<div style=\"display: none;\" class=\"subtitle\">" <> c <> "</div>"
            [Location c]    -> "<div style=\"display: none;\" class=\"location\">" <> c <> "</div>"
            [Year c]        -> "<div style=\"display: none;\" class=\"year\">" <> c <> "</div>"
            [Description c] -> "<div style=\"display: none;\" class=\"description\">" <> c <> "</div>"
            _               -> "<!-- META CONTENT -->"

        helper _ = "<b>[ UNSUPPORTED TAG, PLEASE OPEN A ISSUE AT https://github.com/TheMarkersFoundation/markers ]</b>"