{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Converter.Abnt where

import Data.String.Interpolate.IsString (i)

abntHead :: String -> String -> String
abntHead title lang = [i|
<!DOCTYPE html>
<html lang="#{lang}">
<head>
  <meta charset="UTF-8">
  <title>#{title}</title>

|]

-- 'Times New Roman', Times, serif

abnTex :: String
abnTex = "<link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/npm/mathjax@3/es5/output/chtml/fonts/tex.css\">"

abntPage :: String -> String -> String -> String -> String -> String -> String -> String
abntPage pageSize marginTop marginRight marginBottom marginLeft pageNumberFont pageNumberSize = [i|
  @page {
    size: #{pageSize};
    margin: #{marginTop}cm #{marginRight}cm #{marginBottom}cm #{marginLeft}cm;
    
    /* Cabeçalho padrão com numeração */
    @top-right {
        content: counter(page, decimal);
        font-family: #{pageNumberFont};
        font-size: #{pageNumberSize}pt;
    }
  }
  
  @page noheader {
    @top-right { content: \"\"; }
  }
  
  /* As páginas do conteúdo pré-Sumário não exibem a numeração */
  .pre-summary { page: noheader; }
|]

abntBody :: String -> String -> String -> String -> String -> String
abntBody font titleSize chapterTitleSize textSize lineHeight = [i| 

  body {
      font-family: #{font};
      font-size: #{textSize}pt;
      line-height: #{lineHeight};
      text-align: justify;
      color: #000;
      margin: 0;
      padding: 0;
  }

  h1 { font-size: #{titleSize}pt; }
  h2, h3, h4, h5, h6 {
    text-align: left;
    margin: 1.5em 0 0.5em 0;
    font-size: #{chapterTitleSize}pt;
  }

  p { margin: 0 0 1em 0; }
  .indent { text-indent: 1.25cm; }
|]

abntAbstract :: String -> String -> String -> String
abntAbstract titleAlignment titleSize keywordSpace = [i|
  .abstract p {
    margin: 0;
    text-indent: 0;
    white-space: pre-line;
  }

  .abstract p + p {
        margin-top: #{keywordSpace}em;
  }

  .abstract-title {
        text-align: #{titleAlignment};
        font-size: #{titleSize}pt;
        font-weight: bold;
        margin-bottom: 1em;
  }
|]

--   \    .abnt-code {\n\
--   \      font-family: 'Courier New', Courier, monospace;\n\
--   \      font-size: 10pt;\n\
--   \      padding: 1em;\n\
--   \      margin: 1em 0;\n\
--   \      line-height: 1.2;\n\
--   \      overflow-x: auto;\n\
--   \    }\n\

--   \    \n\

--   \    .container { background-color: white; }\n\
--   \    /* Estilos do Sumário */\n\
--   \    .summary { margin-bottom: 1em; }\n\
--   \    .summary-title { text-align: center; font-size: 14pt; font-weight: bold; margin-bottom: 1em; }\n\
--   \    .summary ul { list-style: none; padding: 0; margin: 0; }\n\
--   \    .summary li {\n\
--   \      font-size: 12pt;\n\
--   \      padding: 0.2em 0;\n\
--   \      display: flex;\n\
--   \      align-items: center;\n\
--   \      white-space: nowrap;\n\
--   \    }\n\
--    \    .summary li .chapter-number {\n\
--   \      margin-right: 5px;\n\
--   \    }\n\
--   \    .summary li .dots {\n\
--   \      flex: 1;\n\
--   \      border-bottom: 1px dotted #000;\n\
--   \      margin: 0 5px;\n\
--   \    }\n\
--   \    .summary li .chapter-title {\n\
--   \      margin-left: 5px;\n\
--   \    }\n\
--   \    .summary li .page {\n\
--   \      margin-left: 5px;\n\
--   \    }\n\
-- \  .abstract { /* seu estilo geral */ }\n\
-- \\
-- \.abbr-item {\n\
-- \  display: flex;\n\
-- \  /* se quiser centralizar verticalmente:\n\
-- \     align-items: center;\n\
-- \  */\n\
-- \}\n\
-- \\
-- \.abbr-name {\n\
-- \  flex: 0 0 120px;   /* largura fixa de 120px (ajuste como precisar) */\n\
-- \  font-weight: bold; /* opcional */\n\
-- \}\n\
-- \\
-- \.abbr-meaning {\n\
-- \  flex: 1;           /* ocupa todo o espaço restante */\n\
-- \  padding-left: 1em; /* espaço extra, se quiser */\n\
-- \}\n\
--     \    .thanks { margin-bottom: 1em; }\n\
--   \    .thanks-title { text-align: center; font-size: 14pt; font-weight: bold; margin-bottom: 1em; }\n\
--   \    .figurelist { margin-bottom: 1em; }\n\
--   \    .figurelist-title { text-align: center; font-size: 14pt; font-weight: bold; margin-bottom: 1em; }\n\
--     \    .figurelist li {\n\
--   \      font-size: 12pt;\n\
--   \      padding: 0.2em 0;\n\
--   \      display: flex;\n\
--   \      align-items: center;\n\
--   \      white-space: nowrap;\n\
--   \    }\n\
--   \    .figurelist li .figure-number {\n\
--   \      margin-left: -6.8vw;\n\
--   \      margin-right: 5px;\n\
--   \    }\n\
--   \    .figurelist li .dots {\n\
--   \      flex: 1;\n\
--   \      border-bottom: 1px dotted #000;\n\
--   \      margin: 0 5px;\n\
--   \    }\n\
--   \    .figurelist li .figure-title {\n\
--   \      margin-left: 5px;}\n\
--   \    .abnt-quote {\n\
--   \      margin: 1.9em 0;\n\
--   \      padding-left: 8cm;\n\
--   \      text-align: justify;\n\
--   \      font-size: 0.9em;\n\
--   \      line-height: 1.2;\n\
--   \    }\n\
--   \    .abnt-quote p {\n\
--   \      text-indent: 0 !important;\n\
--   \      margin: 0;\n\
--   \      padding: 0;\n\
--   \    }\n\
--   \    code {\n\
--   \      background: #eaeaea;\n\
--   \      padding: 0.2em 0.4em;\n\
--   \      border-radius: 4px;\n\
--   \      font-size: 0.8em;\n\
--   \      font-family: 'Fira Code', Consolas, Monaco, 'Andale Mono', 'Ubuntu Mono', monospace;\n\
--   \      font-feature-settings: \"liga\" on, \"calt\" on;\n\
--   \    }\n\
--   \    table {\n\
--   \      width: 100%;\n\
--   \      border-collapse: collapse;\n\
--   \      margin: 1em 0;\n\
--   \    }\n\
--   \    caption { font-size: 1em; text-align: center; margin-bottom: 0.5em; }\n\
--   \    th, td { padding: 8px; text-align: center; border: none; }\n\
--   \    thead th { border: 1px solid #000; font-weight: bold; }\n\
--   \    tbody td { border: 1px solid #000; }\n\
-- \ .math-block {\n\
-- \  font-family: 'MJXZERO', 'Latin Modern Math', serif;\n\
-- \  display: flex;\n\
-- \  font-size: 1.2em;\n\
-- \  line-height: 1;\n\
-- \  vertical-align: middle;\n\
-- \  align-items: center;\n\
-- \  justify-content: center;\n\
-- \  margin: 2em 0;          /* levemente mais compacto */\n\
-- \}\n\
-- \\
-- \/* Fração empilhada estilo TeX */\n\
-- \.math-block .fraction {\n\
-- \  display: inline-flex;\n\
-- \  flex-direction: column;\n\
-- \  align-items: center;\n\
-- \  justify-content: center;\n\
-- \  margin: 0 0.5em;\n\
-- \  font-size: 1.1em;         /* numerador e denominador um pouco menores */\n\
-- \}\n\
-- \.math-block .fraction .num {\n\
-- \  padding: 0 0.1em;\n\
-- \}\n\
-- \.math-block .fraction .den {\n\
-- \  padding: 0 0.1em;\n\
-- \  border-top: 0.07em solid currentColor;\n\
-- \  margin-top: 0.07em;\n\
-- \}\n\
-- \\
-- \/* Expoentes (superscript) */\n\
-- \.math-block sup {\n\
-- \  font-size: 0.7em;\n\
-- \  vertical-align: super;\n\
-- \  line-height: 1;\n\
-- \  margin-left: 0.05em;\n\
-- \}\n\
-- \\
-- \/* Raiz quadrada */\n\
-- \.math-block .radicand {\n\
-- \  display: inline-block;\n\
-- \  position: relative;\n\
-- \  padding-left: 0.5em;      /* espaço antes da raiz */\n\
-- \}\n\
-- \.math-block .radicand::before {\n\
-- \  content: \"\\221A\";\n\
-- \  position: absolute;\n\
-- \  left: 0;\n\
-- \  top: 0;\n\
-- \  font-size: 1em;\n\
-- \  line-height: 1;\n\
-- \}\n\
-- \.math-block .radicand {\n\
-- \  border-top: 0.07em solid currentColor;\n\
-- \  margin-left: -0.1em;\n\
-- \}\n\
-- \\
-- \/* Operadores (mais uniforme) */\n\
-- \.math-block .operator {\n\
-- \  margin: 0 0.2em;\n\
-- \  font-style: normal;\n\
-- \}\n\
-- \\
-- \/* P para probabilidade mantém corpo reto, separador leva espaço */\n\
-- \.math-block .probability {\n\
-- \  font-style: normal;\n\
-- \  margin-right: 0.3em;\n\
-- \}\n\
-- \.math-block .probability .sep {\n\
-- \  margin: 0 0.2em;\n\
-- \}\n\
--   \\
--   \  </style>\n\
--   \  <script src=\"https://unpkg.com/vivliostyle@latest/dist/vivliostyle.js\"></script>\n\
--   \  <script>\n\
--   \    document.addEventListener('DOMContentLoaded', () => {\n\
--   \      // Gera Sumário dinâmico com numeração ABNT\n\
--   \      const summaryDiv = document.querySelector('.summary');\n\
--   \      if (!summaryDiv) return;\n\
--   \      let ul = document.createElement('ul');\n\
--   \      function generateChapterList(chapters, parent = '') {\n\
--   \        chapters.forEach((ch, i) => {\n\
--   \          const num = parent ? parent + '.' + (i+1) : '' + (i+1);\n\
--   \          const h2 = ch.querySelector(':scope > h2'); if (!h2) return;\n\
--   \          const title = h2.textContent.trim();\n\
--   \          h2.innerHTML = `<span class=\"chapter-number\">${num}</span> ${title}`;\n\
--   \          const page = (ch.querySelector('#chapterPageNumber')?.textContent||'');\n\
--   \          const li = document.createElement('li');\n\
--   \          li.innerHTML = `<span class=\"chapter-number\">${num}</span> <span class=\"chapter-title\">${title}</span> <span class=\"dots\"></span> <span class=\"page\">${page}</span>`;\n\
--   \          if ((i+1)%23===0) li.style.pageBreakAfter='always';\n\
--   \          ul.appendChild(li);\n\
--   \          const nested = ch.querySelectorAll(':scope > .chapter');\n\
--   \          if (nested.length) generateChapterList(Array.from(nested), num);\n\
--   \        });\n\
--   \      }\n\
--   \      const tops = Array.from(document.querySelectorAll('.chapter')).filter(c=>!c.closest('.chapter .chapter'));\n\
--   \      generateChapterList(tops);\n\
--   \      if (ul.children.length) summaryDiv.appendChild(ul);\n\
--   \    });\n\
--   \\n\
--   \    document.addEventListener('DOMContentLoaded', () => {\n\
--   \      // Merge <p> dentro de inline tags para manter indentação\n\
--   \      document.querySelectorAll('strong, em, span, code').forEach(inl => {\n\
--   \        const prev = inl.previousElementSibling, next = inl.nextElementSibling;\n\
--   \        if (prev?.tagName==='P' && prev.classList.contains('indent') &&\n\
--   \            next?.tagName==='P' && next.classList.contains('indent')) {\n\
--   \          const merged = document.createElement('p');\n\
--   \          merged.className='indent';\n\
--   \          merged.innerHTML = prev.innerHTML + inl.outerHTML + next.innerHTML;\n\
--   \          next.remove(); inl.remove(); prev.replaceWith(merged);\n\
--   \        }\n\
--   \      });\n\
--   \    });\n\
--   \\n\
--   \    function populateABNTReferences() {\n\
--   \      const list = document.querySelector('.post-summary .references-list');\n\
--   \      if (!list || list.children.length) return;\n\
--   \      document.querySelectorAll('.post-summary span.reference').forEach(ref=>{\n\
-- \        const a = ref.querySelector.bind(ref), fld = (s) => a(s)?.textContent.trim() || '';\n\
--   \        const p=document.createElement('p');\n\
--   \        p.style.textAlign='left'; p.style.lineHeight='1';\n\
--   \        p.innerHTML =\n\
--   \          `${fld('.author')}. <b>${fld('.title').toUpperCase()}</b>, ${fld('.year')}. Disponível em: `+\n\
--   \          `<a style=\"color:black;text-decoration:none;\" href=\"${fld('.url')}\" target=\"_blank\">${fld('.url')}</a>. Acesso em: ${fld('.access')}.`;\n\
--   \        list.appendChild(p);\n\
--   \      });\n\
--   \    }\n\
--   \    document.addEventListener('vivliostyle-rendering-completed', populateABNTReferences);\n\
--   \    document.addEventListener('DOMContentLoaded', populateABNTReferences);\n\
--   \\n\
-- \  document.addEventListener('DOMContentLoaded', () => {\n\
-- \  // Seleciona todas as figuras (evitando figuras aninhadas)\n\
-- \  const figs = Array\n\
-- \    .from(document.querySelectorAll('.figure-item'))\n\
-- \    .filter(f => !f.closest('.figure-item .figure-item'));\n\
-- \\
-- \figs.forEach((fig, i) => {\n\
-- \  const num = i + 1;\n\
-- \  // pega todo o texto bruto do figcaption original\n\
-- \  const rawCaption = fig.querySelector('figcaption')?.textContent || '';\n\
-- \  // separa antes/depois de “Fonte:”\n\
-- \  const [captionText, sourceText] = rawCaption.split('Fonte:');\n\
-- \  const cap = captionText.trim() || 'FIGURA SEM TÍTULO';\n\
-- \  const fonte = sourceText?.trim() || '';\n\
-- \\
-- \  // 1) Atualiza o figcaption de título\n\
-- \  const titleElem = fig.querySelector('.figure-title');\n\
-- \  if (titleElem) {\n\
-- \    // nota o ponto final após o título\n\
-- \    titleElem.textContent = `FIGURA ${num} – ${cap}`;\n\
-- \  }\n\
-- \\
-- \  // 2) Cria ou reutiliza um figcaption para a fonte\n\
-- \  let sourceElem = fig.querySelector('.figure-source');\n\
-- \  if (!sourceElem) {\n\
-- \    sourceElem = document.createElement('figcaption');\n\
-- \    sourceElem.className = 'figure-source';\n\
-- \    sourceElem.style.fontSize = '10pt';\n\
-- \    sourceElem.style.fontStyle = 'italic';\n\
-- \    // insere logo após o figcaption de título\n\
-- \    titleElem.insertAdjacentElement('afterend', sourceElem);\n\
-- \  }\n\
-- \  // só preenche se houver fonte\n\
-- \  sourceElem.textContent = fonte ? `Fonte: ${fonte}` : '';\n\
-- \});\n\
-- \\
-- \\
-- \  // Gera a lista de figuras (sumário)\n\
-- \  const container = document.querySelector('.figurelist');\n\
-- \  if (!container || figs.length === 0) return;\n\
-- \\
-- \  const ul = document.createElement('ul');\n\
-- \  figs.forEach((fig, i) => {\n\
-- \    const num = i + 1;\n\
-- \    const cap = fig.querySelector('.figure-title')?.textContent || 'FIGURA SEM TÍTULO';\n\
-- \    const page = fig.querySelector('.figure-page-number')?.textContent.trim() || '';\n\
-- \    const li = document.createElement('li');\n\
-- \    li.innerHTML =\n\
-- \      `<span class=\"figure-number\">FIGURA ${num}</span>` +\n\
-- \      `<span class=\"figure-title\">${cap.split(' – ')[1].toUpperCase()}</span>` +\n\
-- \      `<span class=\"dots\"></span>` +\n\
-- \      `<span class=\"page\">${page}</span>`;\n\
-- \    if ((i + 1) % 23 === 0) {\n\
-- \      li.style.pageBreakAfter = 'always';\n\
-- \    }\n\
-- \    ul.appendChild(li);\n\
-- \  });\n\
-- \\
-- \  container.appendChild(ul);\n\
-- \});\n\
--   \  </script>\n\
--   \</head>\n\
--   \<body>\n\
--  \ <div class=\"container\">\n\
--   \  <div class=\"pre-summary\">" 