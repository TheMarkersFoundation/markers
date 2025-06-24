{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Converter.To where
import Data.List
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Data.List.Split (splitOn)
import Ast.AbstractSyntaxTree
import Data.Char (isAlpha)
import Data.Maybe (fromMaybe)
import Converter.Helpers
import Converter.Math (renderMath)

import Data.String.Interpolate.IsString (i)

import Converter.Abnt

toAbnt :: Markers -> String
toAbnt (MarkersMain title prefs content) =
  let config = applyPreferences prefs
      (preSections, postSections) = splitSections content
  in header title (lang config)
    <> tex 
    <> openStyle
      <> math
      <> abntPage (paperSize config) (marginTop config) (marginBottom config) (marginLeft config) (marginRight config) (fontFamily config) (fontSize config)
      <> abntBody (fontFamily config) (fontSize config) (chapterSize config) (textSize config) (lineHeight config)
      <> abntThanks (titleAlign config) (titleSize config)
      <> abntAbstract (titleAlign config) (titleSize config) "2"
      <> abbreviations (if boldSectionTitles config then "bold" else "400") (titleAlign config)
      <> abntSummary (titleAlign config) (titleSize config)
                     (if boldSectionTitles config then "bold" else "400") (if titleBold config then "bold" else "400")
      <> abntFigures (figureAlign config) (figureSize config)
                     (if boldSectionTitles config then "bold" else "400") (if figureNumberBold config then "bold" else "400")
      <> abntImageStyle (imageSize config)
      <> abntTables
      <> abntCode
    <> closeStyle
    <> openScript
      <> mergeParagraphs
      <> summaryList (titleBold config) (boldWholeNumber config)
      <> figureList (figureNumberBold config)
      <> references (referencesAlphabetic config)
    <> closeScript
    <> closeHeader
    <> openBody
      <> preSummary
        <> Prelude.foldr (\x acc -> helper x <> acc) "" preSections
      <> closePreSummary
      <> postSummary
        <> Prelude.foldr (\x acc -> helper x <> acc) "" postSections
      <> closePostSummary
    <> closeBody
    <> end
  where
    helper :: MainSection -> String
    helper Tab = "&#x09;"

    helper (Paragraph (Default content))    = [i|<p class="indent">#{content}</p>|]
    helper (Paragraph (Bold content))       = [i|<strong>#{content}</strong>|]
    helper (Paragraph (Italic content))     = [i|<em>#{content}</em>|]
    helper (Paragraph (Underlined content)) = [i|<span style="text-decoration:underline">#{content}</span>|]
    helper (Paragraph (Crossed content))    = [i|<span>#{content}</span>|]
    helper (Paragraph (CodeInline content)) = [i|<code>#{content}</code>|]

    helper (Summary content) = [i|
      <div id="summary" class="summary">
        <h3 class="summary-title">#{content}</h3>
      </div>
    |]

    helper (Thanks content) = [i|
      <div id="thanks" class="thanks">
        <h3 class="thanks-title">AGRADECIMENTOS</h3>
        #{Prelude.foldr (\x acc -> helper x <> acc) "" content}
      </div>
    |]

    helper (Abstract title content) = [i|
      <div id="abstract" class="abstract">
        <h3 class="abstract-title">#{title}</h3>
        #{Prelude.foldr (\x acc -> helper x <> acc) "" content}
      </div>
    |]

    helper (MathBlock expression) = [i|
      <div class="math-block">
        #{foldr (\x acc -> renderMath x <> acc) "" expression}
      </div>
    |]

    helper (Abbreviations title content) = [i|
      <div id="abstract" class="abstract">
        <h3 class="abstract-title">#{title}</h3>
        #{Data.List.concatMap abbHelper content}
      </div>
    |]
      where
        abbHelper (Abbr name meaning) = [i|
        <div class="abbr-item">
          <span class="abbr-name">#{name}</span>
          <span class="abbr-meaning">#{meaning}</span><br>
        </div>
        |]
        abbHelper _ = ""

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

    helper (Figurelist) = [i|
    <div id="figurelist" class="figurelist">
      <h3 class="summary-title">LISTA DE FIGURAS</h3>
    </div>
    |]

    helper (Chap title content) = [i|
    <div class="chapter">
      <h2 style="font-weight: bold;">#{title}</h2>
      #{Prelude.foldr (\x acc -> helper x <> acc) "" content}
    </div>
    |]

    helper (List title content) = [i|
    <div class="chapter">
      <h2 style="font-weight: bold;">#{title}</h2>
      #{Prelude.foldr (\x acc -> helper x <> acc) "" content}
    </div>
    |]

    helper (Abntchapter page title content) = [i|
    <div class="chapter">
      <span id="chapterPageNumber" style="display: none">#{page}</span>
      <h2 style="font-weight: bold;">#{title}</h2>
      #{Prelude.foldr (\x acc -> helper x <> acc) "" content}
    </div>
    |]

    helper Separator = [i|<div class="separator" style="page-break-before: always;"></div>|]

    helper (ImageUrl url content) = [i|
    <div class="figure-item">
      <figure style="text-align:center;">
        <p class="figure-title" style="font-size:12pt; font-style:italic;"></p>
        <img src="#{url}" alt="">
        <figcaption class="figure-source" style="font-size:10pt; font-style:italic;">
          #{Prelude.foldr (\x acc -> helper x <> acc) "" content}
        </figcaption>
        <span class="figure-page-number" style="display:none;">?</span>
      </figure>
    </div>
    |]

    helper (ImageUrlPage page url content) = [i|
    <div class="figure-item">
      <figure style="text-align:center;">
        <p class="figure-title" style="font-size:12pt; font-style:italic;"></p>
        <img src="#{url}" alt="">
        <figcaption class="figure-source" style="font-size:10pt; font-style:italic;">
          #{Prelude.foldr (\x acc -> helper x <> acc) "" content}
        </figcaption>
        <span class="figure-page-number" style="display:none;">#{page}</span>
      </figure>
    </div>
    |]

    helper (Image b64 mimeType content) = [i|
    <div class="figure-item">
      <figure style="text-align:center;">
        <p class="figure-title" style="font-size:12pt; font-style:italic;"></p>
        <img src="data:image/#{mimeType};base64,#{b64}" alt="">
        <figcaption class="figure-source" style="font-size:10pt; font-style:italic;">
          #{Prelude.foldr (\x acc -> helper x <> acc) "" content}
        </figcaption>
        <span class="figure-page-number" style="display:none;">?</span>
      </figure>
    </div>
    |]

    helper (ImagePage page b64 mimeType content) = [i|
    <div class="figure-item">
      <figure style="text-align:center;">
        <p class="figure-title" style="font-size:12pt; font-style:italic;"></p>
        <img src="data:image/#{mimeType};base64,#{b64}" alt="" />
        <figcaption class="figure-source" style="font-size:10pt; font-style:italic;">
          #{Prelude.foldr (\x acc -> helper x <> acc) "" content}
        </figcaption>
        <span class="figure-page-number" style="display:none;">#{page}</span>
      </figure>
    </div>
    |]

    helper (Code content) = [i|
    <pre class="abnt-code">
    #{concatMap extractPlainText content}
    </pre>
    |]
      where
        extractPlainText (Paragraph (Default text)) = text
        extractPlainText _ = Prelude.foldr (\x acc -> helper x <> acc) "" content

    helper (Quote author content) =
      let quoteText = unwords $ map (stripPTags . helper) content
      in [i|<blockquote class="abnt-quote"><p>#{quoteText} #{author}</p></blockquote>|]

    helper (Ref url author title year access content) = [i|
    <p class="indent">
      <span class="reference">
        <span style="visibility: none; display: none" class="title">#{title}</span>
        <span style="visibility: none; display: none" class="author">#{author}</span>
        <span style="visibility: none; display: none" class="year">#{year}</span>
        <span style="visibility: none; display: none" class="access">#{access}</span>
        <span style="visibility: none; display: none" class="url">#{url}</span>
        <span class="content" style="display:inline;">
          #{Prelude.foldr (\x acc -> helper x <> acc) "" content}
        </span>
      </span>
    </p>
    |]

    helper (Link url content) = [i|
    <a href="#{url}">
      #{Prelude.foldr (\x acc -> helper x <> acc) "" content}
    </a>
    |]

    helper (Trace url content) = [i|
    <a href="https://markers.mirvox.xyz/trace/#{url}">
      #{Prelude.foldr (\x acc -> helper x <> acc) "" content}
    </a>
    |]

    helper LineBreak = ""

    helper (Commentary content) = [i|<!-- #{content} -->|]

    helper References = [i|
    <div class="references">
      <span style="visibility: none; display: none" id="referencesPage"></span>
      <h2 class="summary-title">REFERÊNCIAS</h2>
      <div class="references-list"></div>
    </div>
    |]

    helper (ReferencesPaged s) = [i|
    <div class="references">
      <span style="visibility: none; display: none" id="referencesPage">#{s}</span>
      <h2 class="summary-title">REFERÊNCIAS</h2>
      <div class="references-list"></div>
    </div>
    |]

    helper (Table headers rows) =
      let
        ths = concatMap (\x -> [i|<th>#{x}</th>|]) headers
        trs = concatMap
                (\row ->
                  let tds = concatMap (\cell -> [i|<td>#{cell}</td>|]) row
                  in [i|<tr>#{tds}</tr>|]
                ) rows
      in [i|
    <table>
      <thead>
        <tr>#{ths}</tr>
      </thead>
      <tbody>
        #{trs}
      </tbody>
    </table>
    |]

    helper (Meta content) =
      let topAbnt       = Prelude.foldr (\x acc -> helperTopAbnt x <> acc) "" content
          bottomAbnt    = Prelude.foldr (\x acc -> helperBottomAbnt x <> acc) "" content
          secondTop     = Prelude.foldr (\x acc -> helperSecondPageAbntTop x <> acc) "" content
          secondBottom  = Prelude.foldr (\x acc -> helperSecondPageAbntBottom x <> acc) "" content
          secondFooter  = Prelude.foldr (\x acc -> helperSecondPageAbntFooter x <> acc) "" content
      in [i|
    <div class="abnt">
      #{topAbnt}
      <div style="text-align: center;"><h1>#{title}</h1></div>
      #{bottomAbnt}
    </div>
    <div class="separator" style="page-break-before: always;"></div>
    #{secondTop}
    <div style="text-align: center;"><h1>#{title}</h1></div>
    #{secondBottom}
    <div style="padding: 30%;"></div>
    #{secondFooter}
    |]

    helper Empty = ""

    helper _ = "<b>[ UNSUPPORTED TAG, PLEASE OPEN A ISSUE AT https://github.com/TheMarkersFoundation/markers ]</b>"

    helperTopAbnt :: MetaSection -> String
    helperTopAbnt (Institution c) = [i|
    <div style="text-align: center;">
      <p class="institution" style="margin-bottom: 30%">
        <b>#{Prelude.concatMap escapeHtml c}</b>
      </p>
    </div>
    |]

    helperTopAbnt (Author c) = [i|
    <div style="text-align: center;">
      <p class="author" style="margin-bottom: 30%">#{c}</p>
    </div>
    |]

    helperTopAbnt _ = ""

    helperBottomAbnt :: MetaSection -> String
    helperBottomAbnt (Subtitle c) = [i|
    <div style="text-align: center;">
      <p class="subtitle" style="margin-top: -15px; margin-bottom: 55%">#{c}</p>
    </div>
    |]

    helperBottomAbnt (Location c) = [i|
    <div style="text-align: center;">
      <p class="location">#{c}</p>
    </div>
    |]

    helperBottomAbnt (Year c) = [i|
    <div style="text-align: center;">
      <p class="year" style="margin-bottom: 80px">#{c}</p>
    </div>
    |]

    helperBottomAbnt _ = ""

    helperSecondPageAbntTop :: MetaSection -> String
    helperSecondPageAbntTop (Author c) = [i|
    <div style="text-align: center;">
      <p class="author" style="margin-bottom: 30%">#{c}</p>
    </div>
    |]

    helperSecondPageAbntTop _ = ""

    helperSecondPageAbntBottom :: MetaSection -> String
    helperSecondPageAbntBottom (Subtitle c) = [i|
    <div style="text-align: center;">
      <p class="subtitle" style="margin-top: -15px; margin-bottom: 35%">#{c}</p>
    </div>
    |]

    helperSecondPageAbntBottom (Description c) = [i|
    <div style="margin-top: 0px; text-align: justify; margin-right: 0;">
      <p class="description" style="width: 60%; float: right;">#{c}</p>
    </div>
    |]

    helperSecondPageAbntBottom (Mentor m) = [i|
    <div style="margin-top: 0px; text-align: justify; margin-right: 0;">
      <p class="mentor" style="width: 60%; float: right;">
        <br><br><strong>Orientador:</strong> #{m}
      </p>
    </div>
    |]

    helperSecondPageAbntBottom _ = ""

    helperSecondPageAbntFooter :: MetaSection -> String
    helperSecondPageAbntFooter (Location c) = [i|
    <div style="clear: both; text-align: center;">
      <p class="location">#{c}</p>
    </div>
    |]

    helperSecondPageAbntFooter (Year c) = [i|
    <div style="clear: both; text-align: center;">
      <p class="year" style="margin-bottom: 80px">#{c}</p>
    </div>
    |]

    helperSecondPageAbntFooter _ = ""

toMarkdown :: Markers -> String
toMarkdown (MarkersMain titulo _ sections) = "# " <> titulo <> "\n\n" <> Prelude.foldr (\x acc -> helper x <> acc) "" sections
    where
        helper :: MainSection -> String
        helper (Paragraph (Default content)) = content
        helper (Paragraph (Bold content)) = "**" <> content <> "**"
        helper (Paragraph (Italic content)) = "*" <> content <> "*"
        helper (Separator)                          = "---"
        helper (Paragraph (BoldItalic content))     = "***" <> content <> "***"
        helper (Paragraph (Underlined content))     = "**" <> content <> "**" -- Underline is not supported in markdown
        helper (Paragraph (Crossed content))        = "~~" <> content <> "~~"
        helper (Paragraph (CodeInline content))     = "`" <> content <> "`"
        helper (Paragraph (Color _ content))    = "*" <> content <> "*" -- Color is not natively supported in markdown
        helper (Summary content)                    = "**" <> content <> "**" -- Converting to bold header
        helper (Commentary content)                 = "<!-- " <> content <> " -->"  
        helper (Tab)                                = "&#x09;"
        helper (Ref url _ _ _ _ content) = "[" <> Prelude.foldr (\x acc -> helper x <> acc) "" content <> "](" <> url <> ")"
        helper (List title content) = "#### " <> title <> "\n\n" <> Prelude.foldr (\x acc -> helper x <> acc) "" content
        helper (Chap title content) = "### " <> title <> "\n\n" <> Prelude.foldr (\x acc -> helper x <> acc) "" content
        helper (Link url content) = "[" <> Prelude.foldr (\x acc -> helper x <> acc) "" content <> "](" <> url <> ")"
        helper (Trace url content) = "[https://markers.mirvox.xyz/trace/" <> Prelude.foldr (\x acc -> helper x <> acc) "" content <> "](" <> url <> ")"
        helper (ImageUrl url content) = "![" <> Prelude.foldr (\x acc -> helper x <> acc) "" content <> "](" <> url <> ")"
        helper (Image b64 mimeType content)  = "![" <> Prelude.foldr (\x acc -> helper x <> acc) "" content <> "](data:image/" <> mimeType <> ";base64," <> b64 <> ")"
        helper (Video url content) = "[" <> Prelude.foldr (\x acc -> helper x <> acc) "" content <> "](" <> url <> ")" -- Videos need HTML in MD
        helper (Audio url content) = "[" <> Prelude.foldr (\x acc -> helper x <> acc) "" content <> "](" <> url <> ")" -- Audio needs HTML in MD
        helper (Table headers rows) = 
            let headerRow = "|" <> Prelude.foldr (\x acc -> " " <> x <> " |" <> acc) "" headers <> "\n"
                separator = "|" <> Prelude.foldr (\_ acc -> " --- |" <> acc) "" headers <> "\n"
                bodyRows = Prelude.foldr (\row acc -> "|" <> Prelude.foldr (\cell racc -> " " <> cell <> " |" <> racc) "" row <> "\n" <> acc) "" rows
            in headerRow <> separator <> bodyRows
        helper (Code content)
            = "```"
            <> Prelude.foldr (\x acc -> helper x <> acc) "" content
            <> "```"

        helper (LineBreak) = "\n"
        helper _ = "<b>[ UNSUPPORTED TAG, PLEASE OPEN A ISSUE AT https://github.com/TheMarkersFoundation/markers ]</b>"

toHtml :: Markers -> String
toHtml (MarkersMain title _ sections) =
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
        helper (Paragraph (Default content))        = content
        helper (Paragraph (Bold content))           = "<strong>" <> content <> "</strong>"
        helper (Paragraph (Italic content))         = "<em>" <> content <> "</em>"
        helper (Paragraph (Underlined content))     = "<span style=\"text-decoration:underline\">" <> content <> "</span>"
        helper (Paragraph (Crossed content))        = "<s>" <> content <> "</s>"
        helper (Paragraph (BoldItalic content))     = "<b><i>" <> content <> "</i></b>"
        helper (Paragraph (CodeInline content))     = "<code>" <> content <> "</code>"
        helper (Paragraph (Small content))          = "<small>" <> content <> "</small>"
        helper (Paragraph (Top content))            = "<sup>" <> content <> "</sup>"
        helper (Paragraph (Color color content))    = "<b><span style=\"color:" <> color <> "\">" <> content <> "</span></b>"
        helper (Separator)                          = "\n\n<br><hr><br>\n\n"
        helper (Tab)                                = "&#x09;"
        helper (Summary content)                    = "<div><h3>" <> content <> "</h3><div class=\"summary\"></div>"
        helper (Commentary content)                 = "<!-- " <> content <> " -->"  

        helper (Highlighted color content) =
          let inner = concatMap helper content
          in  "<span style=\"background-color:" <> color <> "\">"
              <> inner
              <> "</span>"

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

        helper (Abntchapter _ title content)
            = "\n<div class=\"chapter\"><h2>" <> title <> "</h2>\n"
            <> Prelude.foldr (\x acc -> helper x <> acc) "" content
            <> "</div>\n"

        helper (Quote author content) =
            "<blockquote class=\"modern-quote\">"
            <> "<p>" <> Prelude.foldr (\x acc -> helper x <> acc) "" content <> "</p>"
            <> "<footer><cite>" <> author <> "</cite></footer>"
            <> "</blockquote>"

        helper (Link url content)
            = "\n<a href=\"" <> url <> "\">"
            <> Prelude.foldr (\x acc -> helper x <> acc) "" content
            <> "</a>\n"

        helper (Trace url content)
            = "\n<a style=\"color: #FF4A84\" href=\"/trace/" <> url <> "\">"
            <> Prelude.foldr (\x acc -> helper x <> acc) "" content
            <> "</a>\n"

        helper (Image base64String mimeType content)
            = "\n<img src=\"data:image/" <> mimeType <> ";base64," <> base64String <> "\" alt=\""
            <> Prelude.foldr (\x acc -> helper x <> acc) "" content
            <> "\">\n"

        helper (ImageUrl url content)
            = "\n<img src=\"" <> url <> "\" alt=\""
            <> Prelude.foldr (\x acc -> helper x <> acc) "" content
            <> "\">\n"

        helper (ImagePage _ base64String mimeType content)
            = "\n<img src=\"data:image/" <> mimeType <> ";base64," <> base64String <> "\" alt=\""
            <> Prelude.foldr (\x acc -> helper x <> acc) "" content
            <> "\">\n"

        helper (ImageUrlPage _ url content)
            = "\n<img src=\"" <> url <> "\" alt=\""
            <> Prelude.foldr (\x acc -> helper x <> acc) "" content
            <> "\">\n"

        helper (Video url content)
            = "<center><video src=\"" <> url <> "\" style=\"width: 60%\" controls>\n"
            <> Prelude.foldr (\x acc -> helper x <> acc) "" content
            <> "</video></center>\n"

        helper (Audio url content)
            = "<center><audio src=\"" <> url <> "\" controls>\n"
            <> Prelude.foldr (\x acc -> helper x <> acc) "" content
            <> "</audio></center>\n"

        helper (Code content)
            = "<pre>\n"
            <>  Prelude.concatMap escapeHtml (Prelude.foldr (\x acc -> helper x <> acc) "" content)
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

        helper (LineBreak)
            = "\n<br>\n"
        helper _ = "<b>[ UNSUPPORTED TAG, PLEASE OPEN A ISSUE AT https://github.com/TheMarkersFoundation/markers ]</b>"

toRaw :: Markers -> String
toRaw (MarkersMain someString _ sections) = "<h1>" <> someString <> "</h1>" <> Prelude.foldr (\x acc -> helper x <> acc) "" sections
    where
        helper :: MainSection -> String
        helper (Paragraph (Default content))        = content
        helper (Paragraph (Bold content))           = "<strong>" <> content <> "</strong>"
        helper (Paragraph (Italic content))         = "<em>" <> content <> "</em>"
        helper (Paragraph (Underlined content))     = "<span style=\"text-decoration:underline\">" <> content <> "</span>"
        helper (Paragraph (Crossed content))        = "<s>" <> content <> "</s>"
        helper (Paragraph (Small content))          = "<small>" <> content <> "</small>"
        helper (Paragraph (Top content))            = "<sup>" <> content <> "</sup>"
        helper (Paragraph (BoldItalic content))     = "<b><i>" <> content <> "</i></b>"
        helper (Paragraph (CodeInline content))     = "<code>" <> content <> "</code>"
        helper (Paragraph (Color color content))    = "<b><span class=\"color\" style=\"color: " <> color <> "\" >" <> content <> "</span></b>"
        helper Separator                            = "\n\n<div class=\"separator-container\"><br><hr><br></div>\n\n"
        helper (Commentary content)                 = "<!-- " <> content <> " -->"  
        helper (Tab)                                = "&#x09;"

        helper (List title content)
            = "\n<details><summary>\n" <> title <> "\n</summary>\n"
            <> "<div>" <> Prelude.foldr (\x acc -> helper x <> acc) "" content <> "</div>"
            <> "</details>\n"

        helper (Chap title content)
            = "\n<div class=\"chapter\"><h2>" <> title <> "</h2>\n"
            <> Prelude.foldr (\x acc -> helper x <> acc) "" content
            <> "</div>\n"

        helper (Abntchapter _ title content)
            = "\n<div class=\"chapter\"><h2>" <> title <> "</h2>\n"
            <> Prelude.foldr (\x acc -> helper x <> acc) "" content
            <> "</div>\n"

        helper (Link url content)
            = "\n<a href=\"" <> url <> "\">"
            <> Prelude.foldr (\x acc -> helper x <> acc) "" content
            <> "</a>\n"

        helper (Trace url content)
            = "\n<a style=\"color: #FF4A84\" href=\"/trace/" <> url <> "\">"
            <> Prelude.foldr (\x acc -> helper x <> acc) "" content
            <> "</a>\n"

        helper (Quote author content)
            = "<blockquote><p>" <> Prelude.foldr (\x acc -> helper x <> acc) "" content
            <> "</p><footer>" <> author <> "</footer></blockquote>"

        helper (Image base64String mimeType content)
            = "\n<img src=\"data:image/" <> mimeType <> ";base64," <> base64String <> "\" alt=\""
            <> Prelude.foldr (\x acc -> helper x <> acc) "" content
            <> "\">\n"

        helper (ImageUrl url content)
            = "\n<img src=" <> url <> "\" alt=\""
            <> Prelude.foldr (\x acc -> helper x <> acc) "" content
            <> "\">\n"

        helper (ImagePage _ base64String mimeType content)
            = "\n<img src=\"data:image/" <> mimeType <> ";base64," <> base64String <> "\" alt=\""
            <> Prelude.foldr (\x acc -> helper x <> acc) "" content
            <> "\">\n"

        helper (ImageUrlPage _ url content)
            = "\n<img src=" <> url <> "\" alt=\""
            <> Prelude.foldr (\x acc -> helper x <> acc) "" content
            <> "\">\n"

        helper (Code content)
            = "<pre>\n"
            <> Prelude.foldr (\x acc -> helper x <> acc) "" content
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

        helper (LineBreak)
            = "\n<br>\n"

        helper _ = "<b>[ UNSUPPORTED TAG, PLEASE OPEN A ISSUE AT https://github.com/TheMarkersFoundation/markers ]</b>"
