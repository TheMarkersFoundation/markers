module Converter.To where
import Data.List
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Data.List.Split (splitOn)
import Ast.AbstractSyntaxTree
import Data.Char (isAlpha)
import Data.Maybe (fromMaybe)

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
  in case post of
       []     -> (pre, [])
       (s:ss) -> (pre ++ [s], ss)

isSummary :: MainSection -> Bool
isSummary (Summary _) = True
isSummary _           = False

stripPTags :: String -> String
stripPTags s =
  let withoutOpen  = fromMaybe s (stripPrefix "<p>" s)
      withoutClose = reverse $ fromMaybe (reverse withoutOpen) (stripPrefix (reverse "</p>") (reverse withoutOpen))
  in withoutClose

toAbnt :: Markers -> String
toAbnt (MarkersMain someString sections) =
  let (preSections, postSections) = splitSections sections in
  "<!DOCTYPE html>\n\
  \<html lang=\"pt-BR\">\n\
  \<head>\n\
  \  <meta charset=\"UTF-8\">\n\
  \  <title>" <> someString <> "</title>\n\
  \<link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/npm/mathjax@3/es5/output/chtml/fonts/tex.css\">\
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
  \      line-height: 1.2;\n\
  \      overflow-x: auto;\n\
  \    }\n\
  \    p { margin: 0 0 1em 0; }\n\
\ .abstract p {\n\
\  margin: 0;\n\
\  text-indent: 0;\n\
\  white-space: pre-line; /* preserva quebras de linha e espaços */\n\
\}\n\
\.abstract p + p {\n\
\  margin-top: 2em;\n\
\}\n\
\\
  \    .abstract-title { text-align: center; font-size: 14pt; font-weight: bold; margin-bottom: 1em; }\n\
  \    .indent { text-indent: 1.25cm; }\n\
  \    h1 { font-size: 12pt; }\n\
  \    h2, h3, h4, h5, h6 {\n\
  \      text-align: left;\n\
  \      margin: 1.5em 0 0.5em 0;\n\
  \      font-size: 14pt;\n\
  \    }\n\
  \    .container { background-color: white; }\n\
  \    /* Estilos do Sumário */\n\
  \    .summary { margin-bottom: 1em; }\n\
  \    .summary-title { text-align: center; font-size: 14pt; font-weight: bold; margin-bottom: 1em; }\n\
  \    .summary ul { list-style: none; padding: 0; margin: 0; }\n\
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
\  .abstract { /* seu estilo geral */ }\n\
\\
\.abbr-item {\n\
\  display: flex;\n\
\  /* se quiser centralizar verticalmente:\n\
\     align-items: center;\n\
\  */\n\
\}\n\
\\
\.abbr-name {\n\
\  flex: 0 0 120px;   /* largura fixa de 120px (ajuste como precisar) */\n\
\  font-weight: bold; /* opcional */\n\
\}\n\
\\
\.abbr-meaning {\n\
\  flex: 1;           /* ocupa todo o espaço restante */\n\
\  padding-left: 1em; /* espaço extra, se quiser */\n\
\}\n\
    \    .thanks { margin-bottom: 1em; }\n\
  \    .thanks-title { text-align: center; font-size: 14pt; font-weight: bold; margin-bottom: 1em; }\n\
  \    .figurelist { margin-bottom: 1em; }\n\
  \    .figurelist-title { text-align: center; font-size: 14pt; font-weight: bold; margin-bottom: 1em; }\n\
    \    .figurelist li {\n\
  \      font-size: 12pt;\n\
  \      padding: 0.2em 0;\n\
  \      display: flex;\n\
  \      align-items: center;\n\
  \      white-space: nowrap;\n\
  \    }\n\
  \    .figurelist li .figure-number {\n\
  \      margin-left: -6.8vw;\n\
  \      margin-right: 5px;\n\
  \    }\n\
  \    .figurelist li .dots {\n\
  \      flex: 1;\n\
  \      border-bottom: 1px dotted #000;\n\
  \      margin: 0 5px;\n\
  \    }\n\
  \    .figurelist li .figure-title {\n\
  \      margin-left: 5px;}\n\
  \    .abnt-quote {\n\
  \      margin: 1.9em 0;\n\
  \      padding-left: 8cm;\n\
  \      text-align: justify;\n\
  \      font-size: 0.9em;\n\
  \      line-height: 1.2;\n\
  \    }\n\
  \    .abnt-quote p {\n\
  \      text-indent: 0 !important;\n\
  \      margin: 0;\n\
  \      padding: 0;\n\
  \    }\n\
  \    code {\n\
  \      background: #eaeaea;\n\
  \      padding: 0.2em 0.4em;\n\
  \      border-radius: 4px;\n\
  \      font-size: 0.8em;\n\
  \      font-family: 'Fira Code', Consolas, Monaco, 'Andale Mono', 'Ubuntu Mono', monospace;\n\
  \      font-feature-settings: \"liga\" on, \"calt\" on;\n\
  \    }\n\
  \    table {\n\
  \      width: 100%;\n\
  \      border-collapse: collapse;\n\
  \      margin: 1em 0;\n\
  \    }\n\
  \    caption { font-size: 1em; text-align: center; margin-bottom: 0.5em; }\n\
  \    th, td { padding: 8px; text-align: center; border: none; }\n\
  \    thead th { border: 1px solid #000; font-weight: bold; }\n\
  \    tbody td { border: 1px solid #000; }\n\
\ .math-block {\n\
\  font-family: 'MJXZERO', 'Latin Modern Math', serif;\n\
\  display: flex;\n\
\  font-size: 1.2em;\n\
\  line-height: 1;\n\
\  vertical-align: middle;\n\
\  align-items: center;\n\
\  justify-content: center;\n\
\  margin: 2em 0;          /* levemente mais compacto */\n\
\}\n\
\\
\/* Fração empilhada estilo TeX */\n\
\.math-block .fraction {\n\
\  display: inline-flex;\n\
\  flex-direction: column;\n\
\  align-items: center;\n\
\  justify-content: center;\n\
\  margin: 0 0.5em;\n\
\  font-size: 1.1em;         /* numerador e denominador um pouco menores */\n\
\}\n\
\.math-block .fraction .num {\n\
\  padding: 0 0.1em;\n\
\}\n\
\.math-block .fraction .den {\n\
\  padding: 0 0.1em;\n\
\  border-top: 0.07em solid currentColor;\n\
\  margin-top: 0.07em;\n\
\}\n\
\\
\/* Expoentes (superscript) */\n\
\.math-block sup {\n\
\  font-size: 0.7em;\n\
\  vertical-align: super;\n\
\  line-height: 1;\n\
\  margin-left: 0.05em;\n\
\}\n\
\\
\/* Raiz quadrada */\n\
\.math-block .radicand {\n\
\  display: inline-block;\n\
\  position: relative;\n\
\  padding-left: 0.5em;      /* espaço antes da raiz */\n\
\}\n\
\.math-block .radicand::before {\n\
\  content: \"√\";\n\
\  position: absolute;\n\
\  left: 0;\n\
\  top: 0;\n\
\  font-size: 1em;\n\
\  line-height: 1;\n\
\}\n\
\.math-block .radicand {\n\
\  border-top: 0.07em solid currentColor;\n\
\  margin-left: -0.1em;\n\
\}\n\
\\
\/* Operadores (mais uniforme) */\n\
\.math-block .operator {\n\
\  margin: 0 0.2em;\n\
\  font-style: normal;\n\
\}\n\
\\
\/* P para probabilidade mantém corpo reto, separador leva espaço */\n\
\.math-block .probability {\n\
\  font-style: normal;\n\
\  margin-right: 0.3em;\n\
\}\n\
\.math-block .probability .sep {\n\
\  margin: 0 0.2em;\n\
\}\n\
  \\
  \  </style>\n\
  \  <script src=\"https://unpkg.com/vivliostyle@latest/dist/vivliostyle.js\"></script>\n\
  \  <script>\n\
  \    document.addEventListener('DOMContentLoaded', () => {\n\
  \      // Gera Sumário dinâmico com numeração ABNT\n\
  \      const summaryDiv = document.querySelector('.summary');\n\
  \      if (!summaryDiv) return;\n\
  \      let ul = document.createElement('ul');\n\
  \      function generateChapterList(chapters, parent = '') {\n\
  \        chapters.forEach((ch, i) => {\n\
  \          const num = parent ? parent + '.' + (i+1) : '' + (i+1);\n\
  \          const h2 = ch.querySelector(':scope > h2'); if (!h2) return;\n\
  \          const title = h2.textContent.trim();\n\
  \          h2.innerHTML = `<span class=\"chapter-number\">${num}</span> ${title}`;\n\
  \          const page = (ch.querySelector('#chapterPageNumber')?.textContent||'');\n\
  \          const li = document.createElement('li');\n\
  \          li.innerHTML = `<span class=\"chapter-number\">${num}</span> <span class=\"chapter-title\">${title}</span> <span class=\"dots\"></span> <span class=\"page\">${page}</span>`;\n\
  \          if ((i+1)%23===0) li.style.pageBreakAfter='always';\n\
  \          ul.appendChild(li);\n\
  \          const nested = ch.querySelectorAll(':scope > .chapter');\n\
  \          if (nested.length) generateChapterList(Array.from(nested), num);\n\
  \        });\n\
  \      }\n\
  \      const tops = Array.from(document.querySelectorAll('.chapter')).filter(c=>!c.closest('.chapter .chapter'));\n\
  \      generateChapterList(tops);\n\
  \      if (ul.children.length) summaryDiv.appendChild(ul);\n\
  \    });\n\
  \\n\
  \    document.addEventListener('DOMContentLoaded', () => {\n\
  \      // Merge <p> dentro de inline tags para manter indentação\n\
  \      document.querySelectorAll('strong, em, span, code').forEach(inl => {\n\
  \        const prev = inl.previousElementSibling, next = inl.nextElementSibling;\n\
  \        if (prev?.tagName==='P' && prev.classList.contains('indent') &&\n\
  \            next?.tagName==='P' && next.classList.contains('indent')) {\n\
  \          const merged = document.createElement('p');\n\
  \          merged.className='indent';\n\
  \          merged.innerHTML = prev.innerHTML + inl.outerHTML + next.innerHTML;\n\
  \          next.remove(); inl.remove(); prev.replaceWith(merged);\n\
  \        }\n\
  \      });\n\
  \    });\n\
  \\n\
  \    function populateABNTReferences() {\n\
  \      const list = document.querySelector('.post-summary .references-list');\n\
  \      if (!list || list.children.length) return;\n\
  \      document.querySelectorAll('.post-summary span.reference').forEach(ref=>{\n\
\        const a = ref.querySelector.bind(ref), fld = (s) => a(s)?.textContent.trim() || '';\n\
  \        const p=document.createElement('p');\n\
  \        p.style.textAlign='left'; p.style.lineHeight='1';\n\
  \        p.innerHTML =\n\
  \          `${fld('.author')}. <b>${fld('.title').toUpperCase()}</b>, ${fld('.year')}. Disponível em: `+\n\
  \          `<a style=\"color:black;text-decoration:none;\" href=\"${fld('.url')}\" target=\"_blank\">${fld('.url')}</a>. Acesso em: ${fld('.access')}.`;\n\
  \        list.appendChild(p);\n\
  \      });\n\
  \    }\n\
  \    document.addEventListener('vivliostyle-rendering-completed', populateABNTReferences);\n\
  \    document.addEventListener('DOMContentLoaded', populateABNTReferences);\n\
  \\n\
\  document.addEventListener('DOMContentLoaded', () => {\n\
\  // Seleciona todas as figuras (evitando figuras aninhadas)\n\
\  const figs = Array\n\
\    .from(document.querySelectorAll('.figure-item'))\n\
\    .filter(f => !f.closest('.figure-item .figure-item'));\n\
\\
\figs.forEach((fig, i) => {\n\
\  const num = i + 1;\n\
\  // pega todo o texto bruto do figcaption original\n\
\  const rawCaption = fig.querySelector('figcaption')?.textContent || '';\n\
\  // separa antes/depois de “Fonte:”\n\
\  const [captionText, sourceText] = rawCaption.split('Fonte:');\n\
\  const cap = captionText.trim() || 'FIGURA SEM TÍTULO';\n\
\  const fonte = sourceText?.trim() || '';\n\
\\
\  // 1) Atualiza o figcaption de título\n\
\  const titleElem = fig.querySelector('.figure-title');\n\
\  if (titleElem) {\n\
\    // nota o ponto final após o título\n\
\    titleElem.textContent = `FIGURA ${num} – ${cap}`;\n\
\  }\n\
\\
\  // 2) Cria ou reutiliza um figcaption para a fonte\n\
\  let sourceElem = fig.querySelector('.figure-source');\n\
\  if (!sourceElem) {\n\
\    sourceElem = document.createElement('figcaption');\n\
\    sourceElem.className = 'figure-source';\n\
\    sourceElem.style.fontSize = '10pt';\n\
\    sourceElem.style.fontStyle = 'italic';\n\
\    // insere logo após o figcaption de título\n\
\    titleElem.insertAdjacentElement('afterend', sourceElem);\n\
\  }\n\
\  // só preenche se houver fonte\n\
\  sourceElem.textContent = fonte ? `Fonte: ${fonte}` : '';\n\
\});\n\
\\
\\
\  // Gera a lista de figuras (sumário)\n\
\  const container = document.querySelector('.figurelist');\n\
\  if (!container || figs.length === 0) return;\n\
\\
\  const ul = document.createElement('ul');\n\
\  figs.forEach((fig, i) => {\n\
\    const num = i + 1;\n\
\    const cap = fig.querySelector('.figure-title')?.textContent || 'FIGURA SEM TÍTULO';\n\
\    const page = fig.querySelector('.figure-page-number')?.textContent.trim() || '';\n\
\    const li = document.createElement('li');\n\
\    li.innerHTML =\n\
\      `<span class=\"figure-number\">FIGURA ${num}</span>` +\n\
\      `<span class=\"figure-title\">${cap.split(' – ')[1].toUpperCase()}</span>` +\n\
\      `<span class=\"dots\"></span>` +\n\
\      `<span class=\"page\">${page}</span>`;\n\
\    if ((i + 1) % 23 === 0) {\n\
\      li.style.pageBreakAfter = 'always';\n\
\    }\n\
\    ul.appendChild(li);\n\
\  });\n\
\\
\  container.appendChild(ul);\n\
\});\n\
  \  </script>\n\
  \</head>\n\
  \<body>\n\
 \ <div class=\"container\">\n\
  \  <div class=\"pre-summary\">" 
  <> Prelude.foldr (\x acc -> helper x <> acc) "" preSections
  <> "\n  </div>\n\
  \  <div class=\"post-summary\">"
  <> Prelude.foldr (\x acc -> helper x <> acc) "" postSections
  <> "\n  </div>\n\
  \</div>\n\
  \  </div>\n\
  \</body>\n\
  \</html>"
  where
    -- (mantém todo o `helper` exatamente como antes)

    --------------------------------------------------------------------------------
    -- HELPER: Converte as seções (Markers) para HTML
    --------------------------------------------------------------------------------
    helper :: MainSection -> String
    helper (Paragraph (Default content)) =
      "<p class=\"indent\">" <> content <> "</p>"

    helper (Paragraph (Bold content)) =
      "<strong>" <> content <> "</strong>"

    helper (Paragraph (Italic content)) =
      "<em>" <> content <> "</em>"

    helper (Paragraph (Underlined content)) =
      "<span style=\"text-decoration:underline\">" <> content <> "</span>"
      
    helper (Paragraph (Crossed content)) =
      "<span>" <> content <> "</span>"

    helper (Paragraph (CodeInline content)) = "<code>" <> content <> "</code>"

    helper Tab = "&#x09;"

    helper (Summary content) =
      "<div id=\"summary\" class=\"summary\"><h3 class=\"summary-title\">" <> content <> "</h3></div>"

    helper (Thanks content) =
      "<div id=\"thanks\" class=\"thanks\"><h3 class=\"thanks-title\">AGRADECIMENTOS</h3>"
      <>  Prelude.foldr (\x acc -> helper x <> acc) "" content
      <> "</div>"

    helper (Abstract title content) =
      "<div id=\"abstract\" class=\"abstract\"><h3 class=\"abstract-title\">" <> title <> "</h3>"
      <>  Prelude.foldr (\x acc -> helper x <> acc) "" content
      <> "</div>"

    helper (MathBlock expression) =
      "<div class=\"math-block\">"
      <> foldr (\x acc -> renderMath x <> acc) "" expression
      <> "</div>"
      where
        renderMath expr = case expr of
          Number n        -> n
          Add a b         -> renderBin a " + "  b
          Sub a b         -> renderBin a " − "  b
          Mul a b         -> renderBin a " · "  b
          Div a b         -> renderBin a " ÷ "  b
          ImplicitMul a b -> "<span class=\"implicit-mul\">" <> renderMath a <> renderMath b <> "</span>"
          Ellipsis        -> "<span class=\"ellipsis\">...</span>"
          Parens x        -> "<span class=\"paren\">(" <> renderMath x <> ")</span>"
          PowerOf a b     -> renderMath a <> "<sup>"           <> renderMath b <> "</sup>"
          SquareRoot x    -> "√<span class=\"radicand\">"     <> renderMath x <> "</span>"
          Neg x           -> "<span class=\"operator\">−</span>" <> renderMath x
          Fraction n d ->
            "<span class=\"fraction\">"
            <> "<span class=\"num\">" <> renderMath n <> "</span>"
            <> "<span class=\"den\">" <> renderMath d <> "</span>"
            <> "</span>"
          Var v ->
            let parts = splitOn "_" v 
                base  = head parts
                subs  = tail parts
                sym0  = M.findWithDefault base base greekMap
            in  foldl (\acc sub -> acc ++ "<sub>" ++ sub ++ "</sub>")
                      sym0
                      subs
          Eq a b ->
            renderMath a
            <> "<span class=\"operator\"> = </span>"
            <> renderMath b

          Probability e c ->
            "<span class=\"probability\">"
            <> "<em>P</em>(" <> renderMath e
            <> "<span class=\"sep\">|</span>"
            <> renderMath c
            <> ")</span>"

          Arrow a b ->
            renderMath a
            <> "<span class=\"operator\">→</span>"
            <> renderMath b  
            
          Sum i0 iN body ->
            "<span class=\"sum\">Σ<sub>" <> renderMath i0 <> "</sub>"
            <> "<sup>" <> renderMath iN <> "</sup> " <> renderMath body <> "</span>"

          Product i0 iN body ->
            "<span class=\"prod\">Π<sub>" <> renderMath i0 <> "</sub>"
            <> "<sup>" <> renderMath iN <> "</sup> " <> renderMath body <> "</span>"

          Integral mb body ->
            let bounds = case mb of
                          Nothing      -> ""
                          Just (a,b)   -> "<sub>" <> renderMath a <> "</sub>" <>
                                          "<sup>" <> renderMath b <> "</sup>"
            in "<span class=\"int\">∫" <> bounds <> " " <> renderMath body <> "</span>"

          Limit at body  ->
            "<span class=\"lim\">lim<sub>" <> renderMath at <> "</sub> " <> renderMath body <> "</span>"

          Derivative d body ->
            "<span class=\"deriv\">" <> renderMath d <> " " <> renderMath body <> "</span>"

          Root mb x      ->
            let prefix = case mb of
                          Nothing -> ""
                          Just n  -> "<sup>" <> renderMath n <> "</sup>"
            in prefix <> "√<span class=\"radicand\">" <> renderMath x <> "</span>"

          Binom n k      ->
            "<span class=\"binom\">⎛" <> renderMath n <> "⎞"
            <> "⎝" <> renderMath k <> "⎠</span>"

          Abs x          -> "<span class=\"abs\">|" <> renderMath x <> "|</span>"

          Ellipsis       -> "<span class=\"ellipsis\">...</span>"

          Parens x       -> "(<span class=\"paren\">" <> renderMath x <> "</span>)"

          Vector vs      ->
            "<span class=\"vector\">⟨"
            <> mconcat (intersperse ", " (map renderMath vs))
            <> "⟩</span>"

          Matrix rows    ->
            let renderRow r = "<tr>" <>
                                mconcat (map (\c -> "<td>" <> renderMath c <> "</td>") r)
                                <> "</tr>"
            in "<table class=\"matrix\">" <> mconcat (map renderRow rows) <> "</table>"

          Func f args    ->
            "<span class=\"func\"><em style=\"margin-right: 10px\">" <> f <> "</em>("
            <> mconcat (intersperse ", " (map renderMath args))
            <> ")</span>"

          Piecewise cs   ->
            let renderCase (e,c) = "<div class=\"case\"><span class=\"expr\">"
                                  <> renderMath e <> "</span> if <span class=\"cond\">"
                                  <> renderMath c <> "</span></div>"
            in "<div class=\"cases\">" <> mconcat (map renderCase cs) <> "</div>"
        renderBin a op b =
          renderMath a
          <> "<span class=\"operator\">" <> op <> "</span>"
          <> renderMath b
          
    helper (Abbreviations title content) =
      "<div id=\"abstract\" class=\"abstract\">\n\
      \  <h3 class=\"abstract-title\">" <> title <> "</h3>\n" <>
      Data.List.concatMap abbHelper content <>
      "</div>"
      where
        abbHelper (Abbr name meaning) =
          "  <div class=\"abbr-item\">\n\
          \    <span class=\"abbr-name\">" <> name <> "</span>\n\
          \    <span class=\"abbr-meaning\">" <> meaning <> "</span><br>\n\
          \  </div>\n"
        abbHelper _ = ""

    helper (NumberedList items) =
      "<ol>"
      <> concatMap (\secs ->
          "<li>"
          <> concatMap helper secs
          <> "</li>"
        ) items
      <> "</ol>"

    helper (BulletList items) =
      "<ul>"
      <> concatMap (\secs ->
          "<li>"
          <> concatMap helper secs
          <> "</li>"
        ) items
      <> "</ul>"

    helper (LetteredList items) =
      "<ol type=\"a\">"
      <> concatMap (\secs ->
          "<li>"
          <> concatMap helper secs
          <> "</li>"
        ) items
      <> "</ol>"
      
    helper (Figurelist) =
      "<div id=\"figurelist\" class=\"figurelist\">" <> "<h3 class=\"figurelist-title\">LISTA DE FIGURAS</h3>" <> "</div>"

    helper (Chap title content) =
      "<div class=\"chapter\">\n\
      \<h2 style=\"font-weight: bold;\">" <> title <> "</h2>\n"
      <> Prelude.foldr (\x acc -> helper x <> acc) "" content
      <> "</div>"

    helper (List title content) =
      "<div class=\"chapter\">\n\
      \<h2 style=\"font-weight: bold;\">" <> title <> "</h2>\n"
      <> Prelude.foldr (\x acc -> helper x <> acc) "" content
      <> "</div>"

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

    helper (ImageUrl url content) =
        "<div class=\"figure-item\">"
        <> "\n<figure style=\"text-align:center;\">"
        <> "\n  <p class=\"figure-title\" style=\"font-size:10pt; font-style:italic;\"></p>"
        <> "\n  <img style=\"max-width:100%; height:auto;\" src=\"" <> url <> "\" alt=\"\">"
        <> "\n  <figcaption class=\"figure-source\" style=\"font-size:10pt; font-style:italic;\">"
        <> Prelude.foldr (\x acc -> helper x <> acc) "" content
        <> "</figcaption>"
        <> "\n  <span class=\"figure-page-number\" style=\"display:none;\">"
        <> "?"
        <> "</span>"
        <> "\n</figure>\n"
      <> "</div>"

    helper (ImageUrlPage page url content) =
        "<div class=\"figure-item\">"
        <> "\n<figure style=\"text-align:center;\">"
        <> "\n  <p class=\"figure-title\" style=\"font-size:10pt; font-style:italic;\"></p>"
        <> "\n  <img style=\"max-width:100%; height:auto;\" src=\"" <> url <> "\" alt=\"\">"
        <> "\n  <figcaption class=\"figure-source\" style=\"font-size:10pt; font-style:italic;\">"
        <> Prelude.foldr (\x acc -> helper x <> acc) "" content
        <> "</figcaption>"
        <> "\n  <span class=\"figure-page-number\" style=\"display:none;\">"
        <> page
        <> "</span>"
        <> "\n</figure>\n"
      <> "</div>"

    helper (Image b64 mimeType content) =
      "<div class=\"figure-item\">"
        <> "\n<figure style=\"text-align:center;\">"
        <> "\n  <p class=\"figure-title\" style=\"font-size:10pt; font-style:italic;\"></p>"
        <> "\n  <img style=\"max-width:100%; height:auto;\" src=\"data:image/" <> mimeType <> ";base64," <> b64 <> "\" alt=\"\">"
        <> "\n  <figcaption class=\"figure-source\" style=\"font-size:10pt; font-style:italic;\">"
        <> Prelude.foldr (\x acc -> helper x <> acc) "" content
        <> "</figcaption>"
        <> "\n  <span class=\"figure-page-number\" style=\"display:none;\">"
        <> "?"
        <> "</span>"
        <> "\n</figure>\n"
      <> "</div>"

    helper (ImagePage page b64 mimeType content) =
      "<div class=\"figure-item\">"
        <> "\n<figure style=\"text-align:center;\">"
        <> "\n  <p class=\"figure-title\" style=\"font-size:10pt; font-style:italic;\"></p>"
        <> "\n  <img style=\"max-width:100%; height:auto;\" src=\"data:image/"
        <> mimeType
        <> ";base64,"
        <> b64
        <> "\" alt=\"\" />"
        <> "\n  <figcaption class=\"figure-source\" style=\"font-size:10pt; font-style:italic;\">"
        <> Prelude.foldr (\x acc -> helper x <> acc) "" content
        <> "</figcaption>"
        <> "\n  <span class=\"figure-page-number\" style=\"display:none;\">"
        <> page
        <> "</span>"
        <> "\n</figure>\n"
      <> "</div>"


    helper (Code content) =
      "<pre class=\"abnt-code\">"
      <> concatMap extractPlainText content
      <> "</pre>"
      where
        extractPlainText :: MainSection -> String
        extractPlainText (Paragraph (Default text)) = text
        extractPlainText _ = Prelude.foldr (\x acc -> helper x <> acc) "" content

    helper (Quote author content) =
      let quoteText = unwords $ map (stripPTags . helper) content
      in "<blockquote class=\"abnt-quote\">"
        <> "<p>" <> quoteText <> " " <> author <> "</p>"
        <> "</blockquote>"

    helper (Ref url author title year access content) =
      "<span class=\"reference\">" <>
      "<span style=\"visibility: none; display: none\" class=\"title\">" <> title <> "</span>" <>
      "<span style=\"visibility: none; display: none\" class=\"author\">" <> author <> "</span>" <>
      "<span style=\"visibility: none; display: none\" class=\"year\">" <> year <> "</span>" <>
      "<span style=\"visibility: none; display: none\" class=\"access\">" <> access <> "</span>" <>
      "<span style=\"visibility: none; display: none\" class=\"url\">" <> url <> "</span>" <>
      "<span class=\"content\" style=\"display:inline;\">" <>
          Prelude.foldr (\x acc -> helper x <> acc) "" content <>
      "</span>" <>
      "</span>"

    helper (Link url content)
        = "\n<a href=\"" <> url <> "\">"
        <> Prelude.foldr (\x acc -> helper x <> acc) "" content
        <> "</a>\n"

    helper (Trace url content)
        = "\n<a href=\"https://markers.mirvox.xyz/trace/" <> url <> "\">"
        <> Prelude.foldr (\x acc -> helper x <> acc) "" content
        <> "</a>\n"

    helper LineBreak = ""

    helper (Commentary content)                 = "<!-- " <> content <> " -->"  

    helper References = "<div class=\"references\"><h2 class=\"summary-title\">REFERÊNCIAS</h2><div class=\"references-list\"></div></div>"
    
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

    helper _ = ">unsupported tag??<"
    
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
      "<div style=\"margin-top: 0px; text-align: justify; margin-right: 0;\">"
        <> "<p class=\"description\" style=\"width: 60%; float: right;\">"
        <> c
        <> "</p></div>"
    helperSecondPageAbntBottom (Mentor m) =
      "<div style=\"margin-top: 0px; text-align: justify; margin-right: 0;\">"
        <> "<p class=\"mentor\" style=\"width: 60%; float: right;\">"
        <> "<br><br><strong>Orientador:</strong> " <> m <> "</p></div>"
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
        helper _ = ">unsupported tag??<"

toHtml :: Markers -> String
toHtml (MarkersMain title sections) =
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
        helper _ = ">unsupported tag??<"

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

        helper _ = ">unsupported tag??<"