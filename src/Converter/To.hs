module Converter.To where
import Data.List

import Ast.AbstractSyntaxTree
import Text.Read (Lexeme(String))

escapeHtml :: Char -> String
escapeHtml c = case c of
    '\n' -> "<br>"
    '<' -> "&lt;"
    '>' -> "&gt;"
    '&' -> "&amp;"
    '"' -> "&quot;"
    '\'' -> "&#39;"
    _ -> [c]

splitSections :: [MainSection] -> ([MainSection], [MainSection])
splitSections [] = ([], [])
splitSections xs =
  let (pre, post) = break isSummary xs
  in (pre, post)

isSummary :: MainSection -> Bool
isSummary (Summary _) = True
isSummary _           = False

toAbnt :: Markers -> String
toAbnt (MarkersMain someString sections) =
  let (preSections, postSections) = splitSections sections in
  "<!DOCTYPE html>\n\
  \<html lang=\"pt-BR\">\n\
  \<head>\n\
  \  <meta charset=\"UTF-8\">\n\
  \  <title>" <> someString <> "</title>\n\
  \  <style>\n\
  \    @page {\n\
  \      size: A4;\n\
  \      margin: 3cm 2cm 2cm 3cm;\n\
  \      /* Cabeçalho padrão com numeração */\n\
  \      @top-right {\n\
  \        content: counter(page, decimal);\n\
  \        font-family: 'Times New Roman', Times, serif;\n\
  \        font-size: 12pt;\n\
  \      }\n\
  \    }\n\
  \    @page noheader {\n\
  \      @top-right { content: \"\"; }\n\
  \    }\n\
  \    /* As páginas do conteúdo pré-Sumário não exibem a numeração */\n\
  \    .pre-summary { page: noheader; }\n\
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
  \    .indent {\n\
  \      text-indent: 1.25cm;\n\
  \    }\n\
  \    h1 {\n\
  \      font-size: 12pt;\n\
  \    }\n\
  \    h2, h3, h4, h5, h6 {\n\
  \      text-align: left;\n\
  \      margin: 1.5em 0 0.5em 0;\n\
  \      font-size: 14pt;\n\
  \    }\n\
  \    .container {\n\
  \      background-color: white;\n\
  \    }\n\
  \    /* Estilos do Sumário */\n\
  \    .summary {\n\
  \      margin-bottom: 1em;\n\
  \    }\n\
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
  \.abnt-quote {\
  \  margin: 1em 0;\
  \  padding-left: 4cm; \
  \  text-align: justify;\
  \  font-size: 0.9em; \
  \  line-height: 1.2; \
  \  font-style: normal; \
  \  color: #000;\
  \  background: none;\
  \  border: none;\
  \  }\
  \\
  \  .abnt-quote footer {\
  \  margin-top: 0.5em;\
  \  text-align: right;\
  \  font-weight: normal;\
  \  font-size: 0.9em;\
  \  font-style: normal;\
  \  }\
\\
\ table {\
\  width: 100%;\
\  border-collapse: collapse;\
\  margin: 1em 0;\
\}\
\\
\caption {\
\  font-size: 1em;\
\  text-align: center;\
\  margin-bottom: 0.5em;\
\}\
\\
\th, td {\
\  padding: 8px;\
\  text-align: center;\
\  border: none;\
\}\
\\
\thead th {\
\  border-bottom: 2px solid #000;\
\  font-weight: bold;\
\}\
\\
\tbody td {\
\  border-bottom: 1px solid #000;\
\}\
\\
\tbody tr:last-child td {\
\  border-bottom: none;\
\}\
\\
  \  </style>\n\
  \  <!-- Carrega o Vivliostyle para processamento automático de páginas -->\n\
  \  <script src=\"https://unpkg.com/vivliostyle@latest/dist/vivliostyle.js\"></script>\n\
  \  <script>\n\
  \    // Geração do Sumário\n\
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
  \  <div class=\"container\">\n\
  \    <div class=\"pre-summary\">\n\
  \" <> Prelude.foldr (\x acc -> helper x <> acc) "" preSections <> "\n    </div>\n\
  \    <div class=\"post-summary\">\n\
  \" <> Prelude.foldr (\x acc -> helper x <> acc) "" postSections <> "\n    </div>\n\
  \  </div>\n\
  \</body>\n\
  \</html>"
  where
    --------------------------------------------------------------------------------
    -- HELPER: Converte as seções (Markers) para HTML
    --------------------------------------------------------------------------------
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
    helper (Tab)                                = "&#x09;"

    helper (Summary content) =
      "<div id=\"summary\" class=\"summary\"><h3 class=\"summary-title\">" <> content <> "</h3></div>"
    helper (Abntchapter page title content) =
      "<div class=\"chapter\"><span id=\"chapterPageNumber\" style=\"display: none\">" <> page <> "</span>\n\
      \<h2 style=\"font-weight: bold;\">" <> title <> "</h2>\n"
      <> Prelude.foldr (\x acc -> helper x <> acc) "" content
      <> "</div>"
      
    helper (Meta content) =
      -- CAPA (primeira página)
      "<div class=\"abnt\">\n"
        <> Prelude.foldr (\x acc -> helperTopAbnt x <> acc) "" content
        <> "<div style=\"text-align: center;\"><h1>"
        <> someString
        <> "</h1></div>\n"
        <> Prelude.foldr (\x acc -> helperBottomAbnt x <> acc) "" content
        <> "\n</div>"
        <> "\n<div class=\"separator\" style=\"page-break-before: always;\"></div>"
      -- FOLHA DE ROSTO (segunda página)
        <> Prelude.foldr (\x acc -> helperSecondPageAbntTop x <> acc) "" content
        <> "<div style=\"text-align: center;\"><h1>"
        <> someString
        <> "</h1></div>\n"
        <> Prelude.foldr (\x acc -> helperSecondPageAbntBottom x <> acc) "" content
        <> "<div style=\"padding: 30%;\"></div>"
        <> Prelude.foldr (\x acc -> helperSecondPageAbntFooter x <> acc) "" content

    helper Separator =
      "<div class=\"separator\" style=\"page-break-before: always;\"></div>"
    helper (Image base64String mimeType content) =
      "\n<img src=\"data:image/" <> mimeType <> ";base64," <> base64String <> "\" alt=\""
      <> Prelude.foldr (\x acc -> helper x <> acc) "" content
      <> "\">\n"
    helper (Code content) =
      "<pre class=\"abnt-code\">"
      <> Prelude.foldr (\x acc -> helper x <> acc) "" content
      <> "</pre>"
    helper _ = ""
    
    --------------------------------------------------------------------------------
    -- ABNT Sub-helpers
    --------------------------------------------------------------------------------
    helperTopAbnt :: MetaSection -> String
    helperTopAbnt (Institution c) =
      "<div style=\"text-align: center;\"><p class=\"institution\" style=\"margin-bottom: 30%\"><b>" <> Prelude.concatMap escapeHtml c <> "</b></p></div>"
    helperTopAbnt (Author c) =
      "<div style=\"text-align: center;\"><p class=\"author\" style=\"margin-bottom: 30%\">" <> c <> "</p></div>"
    helperTopAbnt _ = ""
    
    helperBottomAbnt :: MetaSection -> String
    helperBottomAbnt (Subtitle c) =
      "<div style=\"text-align: center;\"><p class=\"subtitle\" style=\"margin-top: -15px; margin-bottom: 55%\">" <> c <> "</p></div>"
    helperBottomAbnt (Location c) =
      "<div style=\"text-align: center;\"><p class=\"location\">" <> c <> "</p></div>"
    helperBottomAbnt (Year c) =
      "<div style=\"text-align: center;\"><p class=\"year\" style=\"margin-bottom: 80px\">" <> c <> "</p></div>"
    helperBottomAbnt _ = "" 

    helperSecondPageAbntTop :: MetaSection -> String
    helperSecondPageAbntTop (Author c) =
      "<div style=\"text-align: center;\"><p class=\"author\" style=\"margin-bottom: 30%\">" <> c <> "</p></div>"
    helperSecondPageAbntTop _ = ""

    helperSecondPageAbntBottom :: MetaSection -> String
    helperSecondPageAbntBottom (Subtitle c) =
      "<div style=\"text-align: center;\"><p class=\"subtitle\" style=\"margin-top: -15px; margin-bottom: 35%\">" <> c <> "</p></div>"
    helperSecondPageAbntBottom (Description c) =
      -- Descrição alinhada à direita, para simular o padrão ABNT
      -- (geralmente no “meio” da página)
      "<div style=\"margin-top: 0px; text-align: justify; margin-right: 0;\">"
        <> "<p class=\"description\" style=\"width: 60%; float: right;\">"
        <> c
        <> "<br><br><strong>Orientador: </strong></p></div>"
    helperSecondPageAbntBottom _ = "" 

    helperSecondPageAbntFooter :: MetaSection -> String
    helperSecondPageAbntFooter (Location c) =
      "<div style=\"clear: both; text-align: center;\"><p class=\"location\">" <> c <> "</p></div>"
    helperSecondPageAbntFooter (Year c) =
      "<div style=\"clear: both; text-align: center;\"><p class=\"year\" style=\"margin-bottom: 80px\">" <> c <> "</p></div>"
    helperSecondPageAbntFooter _ = ""


toMarkdown :: Markers -> String
toMarkdown (MarkersMain titulo sections) = "# " <> titulo <> "\n\n" <> Prelude.foldr (\x acc -> helper x <> acc) "" sections
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
        helper _ = ""

toHtml :: Markers -> String
toHtml (MarkersMain title sections) =
    "<!DOCTYPE html>\
    \<html lang=\"en\">\
    \<head>\
    \  <meta charset=\"UTF-8\">\
    \  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\
    \  <link href=\"https://fonts.googleapis.com/css2?family=Merriweather:wght@400;700&display=swap\" rel=\"stylesheet\">\
    \  <link href=\"https://fonts.googleapis.com/css2?family=Fira+Code&display=swap\" rel=\"stylesheet\">\
    \  <title>" <> title <> "</title>\
    \  <style>\
    \    body {\
    \      margin: 0;\
    \      padding: 0;\
    \      font-family: 'Merriweather', Georgia, 'Times New Roman', Times, serif;\
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
    \  summaryDiv.appendChild(ul);\
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

        helper (Quote author content) =
            "<blockquote class=\"modern-quote\">"
            <> "<p>" <> Prelude.foldr (\x acc -> helper x <> acc) "" content <> "</p>"
            <> "<footer><cite>" <> author <> "</cite></footer>"
            <> "</blockquote>"

        helper (Link url content)
            = "\n<a href=\"" <> url <> "\">"
            <> Prelude.foldr (\x acc -> helper x <> acc) "" content
            <> "</a>\n"

        helper (Image base64String mimeType content)
            = "\n<img src=\"data:image/" <> mimeType <> ";base64," <> base64String <> "\" alt=\""
            <> Prelude.foldr (\x acc -> helper x <> acc) "" content
            <> "\">\n"

        helper (ImageUrl url content)
            = "\n<img src=" <> url <> "\" alt=\""
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

        helper (LineBreak)
            = "\n<br>\n"
        helper _ = ""

toRaw :: Markers -> String
toRaw (MarkersMain someString sections) = "<h1>" <> someString <> "</h1>" <> Prelude.foldr (\x acc -> helper x <> acc) "" sections
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

        helper (Link url content)
            = "\n<a href=\"" <> url <> "\">"
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

        helper _ = ""