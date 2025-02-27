module Converter.To where
import Data.List

import Ast.AbstractSyntaxTree

toAbnt :: Markers -> String
toAbnt (MarkersMain someString sections) =
  "<!DOCTYPE html>\n\
  \<html lang=\"pt-BR\">\n\
  \<head>\n\
  \  <meta charset=\"UTF-8\">\n\
  \  <title>" <> someString <> "</title>\n\
  \  <style>\n\
  \    @page {\n\
  \      size: A4;\n\
  \      margin: 3cm 2cm 2cm 3cm;\n\
  \    }\n\
  \    body {\n\
  \      font-family: 'Times New Roman', Times, serif;\n\
  \      font-size: 12pt;\n\
  \      line-height: 1.5;\n\
  \      text-align: justify;\n\
  \      color: #000;\n\
  \      margin: 0;\n\
  \      padding: 0;\n\
  \    }\n\
  \    .abnt-code {\n\
  \      font-family: 'Courier New', Courier, monospace;\n\
  \      font-size: 10pt;\n\
  \      padding: 1em;\n\
  \      margin: 1em 0;\n\
  \      line-height: 0.2;\n\
  \      overflow-x: auto;\n\
  \    }\n\
  \    p {\n\
  \      margin: 0 0 1em 0;\n\
  \    }\n\
  \    h1 {\n\
  \      font-size: 12pt;\n\
  \    }\n\
  \    .indent {\n\
  \      text-indent: 1.25cm;\n\
  \    }\n\
  \    h2, h3, h4, h5, h6 {\n\
  \      text-align: left;\n\
  \      margin: 1.5em 0 0.5em 0;\n\
  \      font-size: 14pt;\n\
  \    }\n\
  \    .container {\n\
  \      background-color: white;\n\
  \    }\n\
  \    /* Título do sumário */\n\
  \    .summary-title {\n\
  \      text-align: center;\n\
  \      font-size: 14pt;\n\
  \      font-weight: bold;\n\
  \      margin-bottom: 1em;\n\
  \    }\n\
  \    .summary ul {\n\
  \      list-style: none;\n\
  \      padding: 0;\n\
  \      margin: 0;\n\
  \    }\n\
  \    .summary li {\n\
  \      font-size: 12pt;\n\
  \      padding: 0.2em 0;\n\
  \      display: flex;\n\
  \      align-items: center;\n\
  \      white-space: nowrap;\n\
  \    }\n\
  \    .summary li .chapter-number {\n\
  \      margin-right: 5px;\n\
  \    }\n\
  \    .summary li .dots {\n\
  \      flex: 1;\n\
  \      border-bottom: 1px dotted #000;\n\
  \      margin: 0 5px;\n\
  \    }\n\
  \    .summary li .chapter-title {\n\
  \      margin-left: 5px;\n\
  \    }\n\
  \    .summary li .page {\n\
  \      margin-left: 5px;\n\
  \    }\n\
  \  </style>\n\
   \  <script>\n\
  \    document.addEventListener('DOMContentLoaded', () => {\n\
  \      const summaryDiv = document.querySelector('.summary');\n\
  \      if (!summaryDiv) {\n\
  \        console.error('Não foi encontrada a div .summary no DOM.');\n\
  \        return;\n\
  \      }\n\
  \\n\
  \      const ul = document.createElement('ul');\n\
  \\n\
  \      // Função recursiva para gerar a lista de capítulos\n\
  \      function generateChapterList(chapters, parentNumber = '') {\n\
  \        chapters.forEach((chapter, index) => {\n\
  \          const currentNumber = parentNumber ? `${parentNumber}.${index + 1}` : `${index + 1}`;\n\
  \          const h2 = chapter.querySelector(':scope > h2');\n\
  \          if (h2) {\n\
  \            const li = document.createElement('li');\n\
  \            const chapterTitle = h2.textContent.trim();\n\
  \            // Busca o elemento que contém o número da página\n\
  \            const pageElem = chapter.querySelector('#chapterPageNumber');\n\
  \            const pageNumber = pageElem ? pageElem.textContent.trim() : \"\";\n\
  \            li.innerHTML = `<span class=\"chapter-number\">${currentNumber}</span> <span class=\"chapter-title\">${chapterTitle}</span> <span class=\"dots\"></span> <span class=\"page\">${pageNumber}</span>`;\n\
  \            ul.appendChild(li);\n\
  \          }\n\
  \          const nestedChapters = chapter.querySelectorAll(':scope > .chapter');\n\
  \          if (nestedChapters.length > 0) {\n\
  \            generateChapterList(Array.from(nestedChapters), currentNumber);\n\
  \          }\n\
  \        });\n\
  \      }\n\
  \\n\
  \      // Seleciona apenas os capítulos de topo\n\
  \      const topChapters = Array.from(document.querySelectorAll('.chapter')).filter(chapter => {\n\
  \        return !chapter.parentElement.closest('.chapter');\n\
  \      });\n\
  \\n\
  \      generateChapterList(topChapters);\n\
  \      summaryDiv.appendChild(ul);\n\
  \    });\n\
  \  </script>\n\
  \</head>\n\
  \<body>\n\
  \  <div class=\"container\">\n"
  <> Prelude.foldr (\x acc -> helper x <> acc) "" sections
  <> "\n  </div>\n\
  \</body>\n\
  \</html>"
  where
    helper :: MainSection -> String
    helper (Paragraph (Default content)) =
      "<p class=\"indent\">" <> content <> "</p>"
    helper (Paragraph (Bold content)) =
      "<p><strong>" <> content <> "</strong></p>"
    helper (Paragraph (Italic content)) =
      "<p><em>" <> content <> "</em></p>"
    helper (Paragraph (Underlined content)) =
      "<p><span style=\"text-decoration:underline\">" <> content <> "</span></p>"
    helper (Paragraph (Crossed content)) =
      "<p>" <> content <> "</p>"
    helper (Summary content) =
      "<div class=\"summary\"><h3 class=\"summary-title\">" <> content <> "</h3></div>"
    helper (Abntchapter page title content) =
        "<div class=\"chapter\"><span id=\"chapterPageNumber\" style=\"display: none\">" <> page <> "</span><h2 style=\"font-weight: bold;\">" <> title <> "</h2>\n"
        <> Prelude.foldr (\x acc -> helper x <> acc) "" content
        <> "</div>"
    helper (Abnt content) =
      "<div class=\"abnt\">\n"
      <> Prelude.foldr (\x acc -> helperTopAbnt x <> acc) "" content
      <> "<center><h1>" <> someString <> "</h1></center>\n"
      <> Prelude.foldr (\x acc -> helperBottomAbnt x <> acc) "" content
      <> "\n</div>"
    helper Separator = "<br>"
    helper (Image url content) =
      "<div style=\"text-align: center;\">"
      <> Prelude.foldr (\x acc -> helper x <> acc) "" content
      <> "<img src=\"" <> url <> "\" style=\"max-width: 100%; height: auto;\"> </div>"
    helper (Code content) =
      "<pre class=\"abnt-code\">"
      <> Prelude.foldr (\x acc -> helper x <> acc) "" content
      <> "</pre>"
    helper (Page number) =
      "<div id=\"chapter-page\" style=\"page-break-before: always;\">"
      <> "<div id=\"chapter-number\" style=\"display: flex; flex-direction: row; justify-content: flex-end; text-align: right; font-size: 12pt; margin-bottom: 20px;\">"
      <> number
      <> "</div></div>"
    helper _ = ""

    helperTopAbnt :: AbntSection -> String
    helperTopAbnt (Institution content) = "<center><p class=\"institution\" style=\"margin-bottom: 30% \"><b>" <> Prelude.concatMap escapeHtml content <> "</b></p></center>"
    helperTopAbnt (Author content) = "<center><p class=\"author\" style=\"margin-bottom: 30%\">" <> content <> "</p></center>"
    helperTopAbnt _ = ""

    escapeHtml :: Char -> String
    escapeHtml '\n' = "<br>"
    escapeHtml c = [c]

    helperBottomAbnt :: AbntSection -> String
    helperBottomAbnt (Subtitle content) = "<center><p class=\"subtitle\" style=\"margin-top: -15px; margin-bottom: 55%\">" <> content <> "</p></center>"
    helperBottomAbnt (Location content) = "<center><p class=\"location\">" <> content <> "</p></center>"
    helperBottomAbnt (Year content) = "<center><p class=\"year\" style=\"margin-bottom: 80px\">" <> content <> "</p></center>"
    helperBottomAbnt _ = ""


toMarkdown :: Markers -> String
toMarkdown (MarkersMain titulo sections) = "# " <> titulo <> "\n\n" <> Prelude.foldr (\x acc -> helper x <> acc) "" sections
    where
        helper :: MainSection -> String
        helper (Paragraph (Default content)) = content
        helper (Paragraph (Bold content)) = "**" <> content <> "**"
        helper (Paragraph (Italic content)) = "*" <> content <> "*"
        helper (Separator)                          = "---"
        helper (Paragraph (BoldItalic content))     = "***" <> content <> "***"
        helper (Paragraph (Underlined content))     = "**" <> content <> "**" -- Não existe no Markdown. Fallback p/ Italico.
        helper (Paragraph (Crossed content))        = "~~" <> content <> "~~"
        helper (Paragraph (CodeInline content))     = "`" <> content <> "`"
        helper (Ref url _ _ _ _ content) = "[" <> Prelude.foldr (\x acc -> helper x <> acc) "" content <> "](" <> url <> ")"
        helper (List title content) = "#### " <> title <> "\n\n" <> Prelude.foldr (\x acc -> helper x <> acc) "" content
        helper (Chap title content) = "### " <> title <> "\n\n" <> Prelude.foldr (\x acc -> helper x <> acc) "" content
        helper (Link url content) = "[" <> Prelude.foldr (\x acc -> helper x <> acc) "" content <> "](" <> url <> ")"
        helper (Image url content) = "![" <> Prelude.foldr (\x acc -> helper x <> acc) "" content <> "](" <> url <> ")"
        helper (Code content)
            = "```"
            <> Prelude.foldr (\x acc -> helper x <> acc) "" content
            <> "```"
        helper (LineBreak) = "\n"
        helper _ = ""


toHtml :: Markers -> String
toHtml (MarkersMain someString sections) =
    "<!DOCTYPE html>\
    \<html lang=\"en\">\
    \<head>\
    \  <meta charset=\"UTF-8\">\
    \  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\
    \  <title>" <> someString <> "</title>\
    \  <style>\
    \    body {\
    \      margin: 0;\ 
    \      padding: 0;\ 
    \      font-family: -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, \"Helvetica Neue\", Arial, sans-serif;\
    \      background-color: white;\
    \      color: #333;\
    \      text-align: justify;\
    \      text-justify: inter-word;\
    \    }\
    \    \
    \    .container {\
    \      max-width: 800px;\
    \      margin: 0 auto;\
    \      padding: 2em;\
    \      background-color: #fff;\
    \    }\
    \    \
    \    .container h1, .container h2, .container h3 {\
    \      text-align: left;\
    \      margin-top: 1.2em;\
    \      margin-bottom: 0.8em;\
    \    }\
    \    \
    \    .container p {\ 
    \      text-align: justify;\
    \      line-height: 1.6;\
    \      margin: 1em 0;\
    \    } \
    \        details {\
    \            margin-left: 0px; \
    \            margin-top: 10px; \
    \            margin-bottom: 0px; \
    \        } \
    \        summary { \
    \            padding-left: -20px; \
    \            cursor: pointer; \
    \            font-weight: bold; \
    \            padding-top: 5px; \
    \            outline: none; \
    \        } \
    \        details > div, details > details, details > summary { \
    \            margin-left: 10px; \
    \            padding-top: -25px; \
    \        } \
    \    img {\
    \      max-width: 100%;\
    \      height: auto;\
    \      display: block;\
    \      margin: 1em auto;\
    \    }\
    \    \
    \    pre {\
    \      background: #272822;\
    \      color: #f8f8f2;\
    \      padding: 1em;\
    \      overflow-x: auto;\
    \      border-radius: 4px;\
    \      margin: 1em 0;\
    \    }\
    \    code {\
    \      background: #f4f4f4;\
    \      padding: 0.2em 0.4em;\
    \      border-radius: 4px;\
    \    }\
    \  </style>\
    \</head>\
    \<body>\
    \<div class=\"container\">\
    \<h1>" <> someString <> "</h1>"
    <> Prelude.foldr (\x acc -> helper x <> acc) "" sections <>
    "</div>\
    \<script>\n\
    \  document.addEventListener('DOMContentLoaded', () => {\n\
    \    const allDetails = document.querySelectorAll('details');\n\
    \    allDetails.forEach(det => {\n\
    \      det.addEventListener('toggle', () => {\n\
    \      });\n\
    \    });\n\
    \  });\n\
    \document.querySelectorAll('.chapter h2').forEach(h2 => {\n\
        \const level = h2.closest('.chapter').parentElement?.closest('.chapter') ? 2 : 1;\n\
        \if (level === 2) {\n\
            \h2.style.fontSize = '1.2em';\n\
        \} else {\n\
            \h2.style.fontSize = '1.5em';\n\
        \}\n\
    \});\n\
    \const summaryDiv = document.querySelector('.summary');\n\
    \const chapters = document.querySelectorAll('.chapter h2');\n\
    \const ul = document.createElement('ul');\n\
    \\n\
    \chapters.forEach(chapter => {\n\
    \    const li = document.createElement('li');\n\
    \    li.textContent = chapter.textContent;\n\
    \    ul.appendChild(li);\n\
    \});\n\
    \\n\
    \summaryDiv.appendChild(ul);\n\
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
        helper (Paragraph (Color color content))    = "<b><span style=\"color:" <> color <> "\">" <> content <> "</span></b>"
        helper (Separator)                          = "\n\n<br><hr><br>\n\n"
        helper (Summary content)                    = "<div><h3>" <> content <> "</h3><div class=\"summary\"></div>"

        helper (Ref url _ title _ _ _)
            = "<a href=\"" <> url <> "\">" <> title <> "</a>"

        helper (List title content)
            = "\n<details><summary>\n" <> title <> "\n</summary>\n"
            <> "<div>" <> Prelude.foldr (\x acc -> helper x <> acc) "" content <> "</div>"
            <> "</details>\n"

        helper (Chap title content)
            = "\n<div class=\"chapter\"><h2>" <> title <> "</h2>\n"
            <> Prelude.foldr (\x acc -> helper x <> acc) "" content
            <> "</div>\n"

        helper (Link url content)
            = "\n<a href=\"" <> url <> "\">"
            <> Prelude.foldr (\x acc -> helper x <> acc) "" content
            <> "</a>\n"

        helper (Image url content)
            = "\n<img src=\"" <> url <> "\" alt=\""
            <> Prelude.foldr (\x acc -> helper x <> acc) "" content
            <> "\">\n"

        helper (Video url content)
            = "<center><video src=\"" <> url <> "\" style=\"width: 60%\" controls>\n"
            <> Prelude.foldr (\x acc -> helper x <> acc) "" content
            <> "</video></center>\n"

        helper (Iframe url content)
            = "<center><iframe src=\"" <> url <> "\">\n"
            <> Prelude.foldr (\x acc -> helper x <> acc) "" content
            <> "</iframe></center>\n"

        helper (Audio url content)
            = "<center><audio src=\"" <> url <> "\" controls>\n"
            <> Prelude.foldr (\x acc -> helper x <> acc) "" content
            <> "</audio></center>\n"

        helper (Code content)
            = "<pre>\n"
            <> Prelude.foldr (\x acc -> helper x <> acc) "" content
            <> "</pre>\n"

        helper (LineBreak)
            = "\n<br>\n"

        helper _ = ""

toJson :: Markers -> String
toJson (MarkersMain someString sections) = "{\n\t\"title\": \"" <> escapeJson someString <> "\",\n\t\"main\": [" <> processSections sections <> "\n\t]\n}"    where
    processSections :: [MainSection] -> String
    processSections [] = ""
    processSections sections =
        "\n\t\t" <> Data.List.intercalate ",\n\t\t" (Prelude.map helper sections)

    helper :: MainSection -> String
    helper Empty = "{}"
    helper (Paragraph (Default content)) =
        "{\"defaultText\": \"" <> escapeJson content <> "\"}"
    helper (Paragraph (Bold content)) =
        "{\"boldText\": \"" <> escapeJson content <> "\"}"
    helper (Paragraph (Italic content)) =
        "{\"italicText\": \"" <> escapeJson content <> "\"}"
    helper (Paragraph (Underlined content)) =
        "{\"underlinedText\": \"" <> escapeJson content <> "\"}"
    helper (Paragraph (Crossed content)) =
        "{\"crossedText\": \"" <> escapeJson content <> "\"}"
    helper (Paragraph (CodeInline content)) =
        "{\"inlineCode\": \"" <> escapeJson content <> "\"}"
    helper (Ref url author title year access content) =
        "{\"reference\": {\"url\": \"" <> escapeJson url 
        <> "\", \"author\": \"" <> escapeJson author 
        <> "\", \"title\": \"" <> escapeJson title 
        <> "\", \"year\": \"" <> escapeJson year 
        <> "\", \"access\": \"" <> escapeJson access 
        <> "\", \"content\": [" <> processSections content <> "]}}"
    helper (List title content) =
        "{\"listTitle\": \"" <> escapeJson title <> "\", \"items\": [" <> processSections content <> "]}"
    helper (Chap title content) =
        "{\"chapterTitle\": \"" <> escapeJson title <> "\", \"chapterContent\": [" <> processSections content <> "]}"
    helper (Link url content) =
        "{\"link\": {\"url\": \"" <> escapeJson url <> "\", \"text\": [" <> processSections content <> "]}}"
    helper (Image url content) =
        "{\"image\": {\"url\": \"" <> escapeJson url <> "\", \"alt\": [" <> processSections content <> "]}}"
    helper (Code content) =
        "{\"code\": [" <> processSections content <> "]}"
    helper LineBreak =
        "{\"lineBreak\": true}"
    helper _ = ""

    escapeJson :: String -> String
    escapeJson = Prelude.concatMap escapeChar
        where
        escapeChar :: Char -> String
        escapeChar c = case c of
            '"'  -> "\\\""
            '\\' -> "\\\\"
            '\n' -> "\\n"
            '\r' -> "\\r"
            '\t' -> "\\t"
            '\b' -> "\\b"
            '\f' -> "\\f"
            x    -> [x]

    removeTrailingComma :: String -> String
    removeTrailingComma s = if not (Prelude.null s) && Prelude.last s == ',' then Prelude.init s else s

toRaw :: Markers -> String
toRaw (MarkersMain someString sections) = "<h1>" <> someString <> "</h1>" <> Prelude.foldr (\x acc -> helper x <> acc) "" sections
    where
        helper :: MainSection -> String
        helper (Paragraph (Default content))        = content
        helper (Paragraph (Bold content))           = "<strong>" <> content <> "</strong>"
        helper (Paragraph (Italic content))         = "<em>" <> content <> "</em>"
        helper (Paragraph (Underlined content))     = "<span style=\"text-decoration:underline\">" <> content <> "</span>"
        helper (Paragraph (Crossed content))        = "<s>" <> content <> "</s>"
        helper (Paragraph (BoldItalic content))     = "<b><i>" <> content <> "</i></b>"
        helper (Paragraph (CodeInline content))     = "<code>" <> content <> "</code>"
        helper (Paragraph (Color color content))    = "<b><span class=\"color\" style=\"color: " <> color <> "\" >" <> content <> "</span></b>"
        helper Separator                            = "\n\n<div class=\"separator-container\"><br><hr><br></div>\n\n"

        helper (List title content)
            = "\n<details><summary>\n" <> title <> "\n</summary>\n"
            <> "<div>" <> Prelude.foldr (\x acc -> helper x <> acc) "" content <> "</div>"
            <> "</details>\n"

        helper (Chap title content)
            = "\n<div class=\"chapter\"><h2>" <> title <> "</h2>\n"
            <> Prelude.foldr (\x acc -> helper x <> acc) "" content
            <> "</div>\n"

        helper (Link url content)
            = "\n<a href=\"" <> url <> "\">"
            <> Prelude.foldr (\x acc -> helper x <> acc) "" content
            <> "</a>\n"

        helper (Image url content)
            = "\n<img src=\"" <> url <> "\" alt=\""
            <> Prelude.foldr (\x acc -> helper x <> acc) "" content
            <> "\">\n"

        helper (Code content)
            = "<pre>\n"
            <> Prelude.foldr (\x acc -> helper x <> acc) "" content
            <> "</pre>\n"

        helper (LineBreak)
            = "\n<br>\n"

        helper _ = ""