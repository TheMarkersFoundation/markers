{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Converter.Abnt where

import Data.String.Interpolate.IsString (i)

-- 'Times New Roman', Times, serif

-- default: "A4" "3" "2" "2" "3" "\'Times New Roman\', Times, serif" "12"
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
      @top-right { content: ""; }
    }
    
    /* As páginas do conteúdo pré-Sumário não exibem a numeração */
    .pre-summary { page: noheader; }

    .container { background-color: white; }
|]

-- default: "\'Times New Roman\', Times, serif" "12" "14" "12" "1.5"
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

abntThanks :: String -> String -> String
abntThanks titleAlignment titleSize = [i|
    .thanks { margin-bottom: 1em; }
    .thanks-title { text-align: #{titleAlignment}; font-size: #{titleSize}pt; font-weight: bold; margin-bottom: 1em; }
|]

-- default: "bold" "left"
abbreviations :: String -> String -> String
abbreviations titleWeight alignment = [i|
    .abbr-item {
      display: flex;
      align-items: #{alignment};
    }

    .abbr-name {
    flex: 0 0 120px;
    font-weight: #{titleWeight};
    }

    .abbr-meaning {
    flex: 1;
    padding-left: 1em;
    }
|]

abntCode :: String
abntCode = [i|
    .abnt-code {
      font-family: 'Courier New', Courier, monospace;
      font-size: 10pt;
      padding: 1em;
      margin: 1em 0;
      line-height: 1.2;
      overflow-x: auto;
    }

    code {
      background: #eaeaea;
      padding: 0.2em 0.4em;
      border-radius: 4px;
      font-size: 0.8em;
      font-family: 'Fira Code', Consolas, Monaco, 'Andale Mono', 'Ubuntu Mono', monospace;
      font-feature-settings: "liga" on, "calt" on;
    }
|]

abntTables :: String
abntTables = [i|
    table {
      width: 100%;
      border-collapse: collapse;
      margin: 1em 0;
    }
    th, td { padding: 8px; text-align: center; border: none; }
    thead th { border: 1px solid #000; font-weight: bold; }
    tbody td { border: 1px solid #000; }
|]

abntQuotes :: String
abntQuotes = [i|
    .abnt-quote {
      margin: 1.9em 0;
      padding-left: 8cm;
      text-align: justify;
      font-size: 0.9em;
      line-height: 1.2;
    }

    .abnt-quote p {
      text-indent: 0 !important;
      margin: 0;
      padding: 0;
    }

    caption {
      font-size: 1em;
      text-align: center;
      margin-bottom: 0.5em;
    }
|]

-- default: "center" "14" "2em"
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

-- default: center 14 bold 400
abntSummary :: String -> String -> String -> String -> String
abntSummary summaryTitleAlign summaryTitleFontSize summaryTitleWeight chapterTitleWeight = [i|
    .summary { margin-bottom: 1em; }
    .summary-title { text-align: #{summaryTitleAlign}; font-size: #{summaryTitleFontSize}pt; font-weight: #{summaryTitleWeight}; margin-bottom: 1em; }
    .summary ul { list-style: none; padding: 0; margin: 0; }
    .summary li {
      font-size: 12pt;
      padding: 0.2em 0;
      display: flex;
      align-items: center;
      white-space: nowrap;
    }

    .summary li .chapter-number {
      margin-right: 5px;
    }

    .summary li .dots {
      flex: 1;
      border-bottom: 1px dotted #000;
      margin: 0 5px;
    }

    .summary li .chapter-title {
      margin-left: 5px;
      font-weight: #{chapterTitleWeight};
    }

    .summary li .page {
      margin-left: 5px;
    }
|]

abntFigures :: String -> String -> String -> String -> String
abntFigures figureTitleAlign figureTitleFontSize figureTitleWeight figureSourceTitleWeight = [i|
    .figurelist { margin-bottom: 1em; }
    .figurelist-title { text-align: #{figureTitleAlign}; font-size: #{figureTitleFontSize}pt; font-weight: #{figureTitleWeight}; margin-bottom: 1em; }
    .figurelist li {
      font-size: 12pt;
      padding: 0.2em 0;
      display: flex;
      align-items: center;
      white-space: nowrap;
    }

    .figurelist li .figure-number {
      margin-left: -6.8vw;
      margin-right: 5px;
    }

    .figurelist li .dots {
      flex: 1;
      border-bottom: 1px dotted #000;
      margin: 0 5px;
    }

    .figurelist li .figure-title {
      margin-left: 5px;
      font-weight: #{figureSourceTitleWeight};
    }
|]

abntEquations :: String -> String -> String -> String -> String
abntEquations equationTitleAlign equationTitleFontSize equationTitleWeightt equationTitleWeight = [i|
    .equationlist { margin-bottom: 1em; }
    .equationlist-title { text-align: #{equationTitleAlign}; font-size: #{equationTitleFontSize}pt; font-weight: #{equationTitleWeight}; margin-bottom: 1em; }
    .equationlist li {
      font-size: 12pt;
      padding: 0.2em 0;
      display: flex;
      align-items: center;
      white-space: nowrap;
    }

    .equationlist li .figure-number {
      margin-left: -6.8vw;
      margin-right: 5px;
    }

    .equationlist li .dots {
      flex: 1;
      border-bottom: 1px dotted #000;
      margin: 0 5px;
    }

    .equationlist li .figure-title {
      margin-left: 5px;
      font-weight: #{equationTitleWeight};
    }
|]

abntImageStyle :: String -> String
abntImageStyle size = [i|
  .figure-item {
    break-inside: avoid;
    page-break-inside: avoid;
    display: block;
  }

  .figure-item img {
    max-width: #{size}%;
    height: auto;
|]

mergeParagraphs :: String
mergeParagraphs = [i|
  document.addEventListener('DOMContentLoaded', () => {
    document.querySelectorAll('li p.indent').forEach(p => {
      p.classList.remove('indent');
    });
    
    document.querySelectorAll('strong, em, span, code').forEach(inl => {
      const prev = inl.previousElementSibling, next = inl.nextElementSibling;
      if (prev?.tagName === 'P' && prev.classList.contains('indent') &&
          next?.tagName === 'P' && next.classList.contains('indent') &&
          prev.parentElement.tagName !== 'LI' && next.parentElement.tagName !== 'LI') {
        const merged = document.createElement('p');
        merged.className = 'indent';
        merged.innerHTML = prev.innerHTML + inl.outerHTML + next.innerHTML;
        next.remove(); inl.remove(); prev.replaceWith(merged);
      }
    });
  });
|]

figureList :: Bool -> String
figureList hasBoldNumbering = [i|
  document.addEventListener('DOMContentLoaded', () => {

    const hasBoldNumbering = #{if hasBoldNumbering then "true" else "false"};

    // Seleciona todas as figuras (evitando figuras aninhadas)
    const figs = Array.from(document.querySelectorAll('.figure-item'))
      .filter(f => !f.closest('.figure-item .figure-item'));

    figs.forEach((fig, i) => {
      const num = i + 1;
      // Pega todo o texto bruto do figcaption original
      const rawCaption = fig.querySelector('figcaption')?.textContent || '';
      // Separa antes/depois de "Fonte:"
      const parts = rawCaption.split('Fonte:');
      const captionText = parts[0] ? parts[0].trim() : '';
      const sourceText  = parts[1] ? parts[1].trim()  : '';
      const cap = captionText || 'FIGURA SEM TÍTULO';
      const fonte = sourceText;

      // 1) Atualiza o figcaption de título
      const titleElem = fig.querySelector('.figure-title');
      if (titleElem) {
        // Nota: adiciona o número da figura e o título
        titleElem.textContent = `FIGURA ${num} – ${cap}`;
      }

      // 2) Cria ou reutiliza um figcaption para a fonte
      let sourceElem = fig.querySelector('.figure-source');
      if (!sourceElem) {
        sourceElem = document.createElement('figcaption');
        sourceElem.className = 'figure-source';
        sourceElem.style.fontSize = '10pt';
        sourceElem.style.fontStyle = 'italic';
        // Insere logo após o figcaption de título
        titleElem.insertAdjacentElement('afterend', sourceElem);
      }
      // Só preenche se houver fonte
      sourceElem.textContent = fonte ? `Fonte: ${fonte}` : '';
    });

    // Gera a lista de figuras (sumário)
    const container = document.querySelector('.figurelist');
    if (!container || figs.length === 0) return;

    const ul = document.createElement('ul');
    figs.forEach((fig, i) => {
      const num = i + 1;
      // Pega o título da figura e extrai a parte após " – "
      const capText = fig.querySelector('.figure-title')?.textContent || 'FIGURA SEM TÍTULO';
      const splitParts = capText.split(' – ');
      const textForTitle = splitParts[1] ? splitParts[1].toUpperCase() : '';
      const page = fig.querySelector('.figure-page-number')?.textContent.trim() || '';
      const li = document.createElement('li');
      
      if(hasBoldNumbering) {
        li.innerHTML =
          `<span class="figure-number"><strong>FIGURA ${num}</strong></span>` +
          `<span class="figure-title">${textForTitle}</span>` +
          `<span class="dots"></span>` +
          `<span class="page">${page}</span>`;
      } else {
        li.innerHTML =
          `<span class="figure-number">FIGURA ${num}</span>` +
          `<span class="figure-title">${textForTitle}</span>` +
          `<span class="dots"></span>` +
          `<span class="page">${page}</span>`;
      }
        
      // Força quebra de página a cada 23 itens
      if ((i + 1) % 23 === 0) {
        li.style.pageBreakAfter = 'always';
      }
      ul.appendChild(li);
    });

    container.appendChild(ul);
  });
|]

equationList :: String
equationList = [i|
  document.addEventListener('DOMContentLoaded', () => {

    // Select all math-equations (assumed to have the class "math-block")
    const eqs = Array.from(document.querySelectorAll('.math-block'));
    if (eqs.length === 0) return;

    // Generate the equation list (summary)
    const container = document.querySelector('.equationlist');
    if (!container || eqs.length === 0) return;

    const ul = document.createElement('ul');
    
    eqs.forEach((eq, i) => {
      const num = i + 1;
      const page = eq.querySelector('#mathPageNumber')?.textContent.trim() || '';
      // Get the equation id (if defined)
      const eqId = eq.getAttribute('id') || '';
      
      const li = document.createElement('li');
      li.innerHTML =
        `<span class="figure-number">EQUAÇÃO ${num}</span>` +
        `<span class="figure-title">${eqId}</span>` +
        `<span class="dots"></span>` +
        `<span class="page">${page}</span>`;
      
      // Force a page break after every 23 items
      if ((i + 1) % 23 === 0) {
        li.style.pageBreakAfter = 'always';
      }
      ul.appendChild(li);
    });
    
    container.appendChild(ul);
  });
|]

summaryList :: Bool -> Bool -> String
summaryList hasBoldNumbering hasBoldWholeNumbers = [i|
    document.addEventListener('DOMContentLoaded', () => {
      const hasBoldNumbering = #{if hasBoldNumbering then "true" else "false"};
      const hasBoldWholeNumbers = #{if hasBoldWholeNumbers then "true" else "false"};

      const summaryDiv = document.querySelector('.summary');
      if (!summaryDiv) return;
      
      let ul = document.createElement('ul');

      function generateChapterList(chapters, parent = '') {
        chapters.forEach((ch, i) => {
          const num = parent ? `${parent}.${i + 1}` : `${i + 1}`;
          const h2 = ch.querySelector(':scope > h2');
          if (!h2) return;
          const title = h2.textContent.trim();
          h2.innerHTML = `<span class="chapter-number">${num}</span> ${title}`;
          const page = (ch.querySelector('#chapterPageNumber')?.textContent || '');
          
          const li = document.createElement('li');

          if (hasBoldNumbering) {
            li.innerHTML = `<span class="chapter-number"><strong>${num}</strong></span> <span class="chapter-title">${title}</span> <span class="dots"></span> <span class="page">${page}</span>`;
          } else if (hasBoldWholeNumbers) {
            if (num.indexOf('.') === -1) {
              li.innerHTML = `<span class="chapter-number"><strong>${num}</strong></span> <span class="chapter-title"><strong>${title}</strong></span> <span class="dots"></span> <span class="page">${page}</span>`;
            } else {
              li.innerHTML = `<span class="chapter-number">${num}</span> <span class="chapter-title">${title}</span> <span class="dots"></span> <span class="page">${page}</span>`;
            }
          } else {
            li.innerHTML = `<span class="chapter-number">${num}</span> <span class="chapter-title">${title}</span> <span class="dots"></span> <span class="page">${page}</span>`;
          }

          if ((i + 1) % 23 === 0) li.style.pageBreakAfter = 'always';
          ul.appendChild(li);

          const nested = ch.querySelectorAll(':scope > .chapter');
          if (nested.length) generateChapterList(Array.from(nested), num);
        });
      }

      // Select top-level chapters (excluding nested chapters)
      const tops = Array.from(document.querySelectorAll('.chapter'))
        .filter(c => !c.closest('.chapter .chapter'));
      const topCount = tops.length;
      generateChapterList(tops);

      const refPage = document.querySelector('#referencesPage')?.textContent || '';
      const liRef = document.createElement('li');
      const refNum = (topCount + 1).toString();
      
      let refInner = "";
      if (hasBoldNumbering) {
        refInner = `<span class="chapter-number"><strong>${refNum}</strong></span> <span class="chapter-title">REFERÊNCIAS</span> <span class="dots"></span> <span class="page">${refPage}</span>`;
      } else if (hasBoldWholeNumbers) {
        if (refNum.indexOf('.') === -1) {
          refInner = `<span class="chapter-number"><strong>${refNum}</strong></span> <span class="chapter-title"><strong>REFERÊNCIAS</strong></span> <span class="dots"></span> <span class="page">${refPage}</span>`;
        } else {
          refInner = `<span class="chapter-number">${refNum}</span> <span class="chapter-title">REFERÊNCIAS</span> <span class="dots"></span> <span class="page">${refPage}</span>`;
        }
      } else {
        refInner = `<span class="chapter-number">${refNum}</span> <span class="chapter-title">REFERÊNCIAS</span> <span class="dots"></span> <span class="page">${refPage}</span>`;
      }
      
      liRef.innerHTML = refInner;
      ul.appendChild(liRef);

      if (ul.children.length) summaryDiv.appendChild(ul);
    });
|]

references :: Bool -> String
references alphabetic = [i|
    document.addEventListener('DOMContentLoaded', () => {
      function populateABNTReferences() {
        // Flag coming from the template: true means alphabetically sort, false means keep natural order
        const alphabetic = #{if alphabetic then "true" else "false"};

        const list = document.querySelector('.post-summary .references-list');
        if (!list || list.children.length) return;
      
        // Collect all reference elements
        let refs = Array.from(document.querySelectorAll('.post-summary span.reference'));
      
        // If alphabetic is true, sort the references by the author text (you could also sort by title)
        if (alphabetic) {
          refs.sort((refA, refB) => {
            const getField = (ref, selector) => {
              const el = ref.querySelector(selector);
              return el ? el.textContent.trim() : "";
            };
            const authorA = getField(refA, '.author').toLowerCase();
            const authorB = getField(refB, '.author').toLowerCase();
            return authorA.localeCompare(authorB);
          });
        }
      
        // Process each reference and append to the list
        refs.forEach(ref => {
          // Bind querySelector to the current reference element
          const a = ref.querySelector.bind(ref);
          // Helper function to get a field's text content
          const fld = (s) => {
            const el = a(s);
            return el ? el.textContent.trim() : '';
          };
      
          const p = document.createElement('p');
          p.style.textAlign = 'left';
          p.style.lineHeight = '1';
      
          p.innerHTML = 
            `${fld('.author')}. <b>${fld('.title').toUpperCase()}</b>, ${fld('.year')}. Disponível em: ` +
            `<a style="color:black;text-decoration:none;" href="${fld('.url')}" target="_blank">${fld('.url')}</a>. ` +
            `Acesso em: ${fld('.access')}.`;
      
          list.appendChild(p);
        });
      }
      populateABNTReferences();
    });
|]

preSummary :: String
preSummary = [i|
<div class="container">
<div class="pre-summary">
|]

closePreSummary :: String
closePreSummary = [i|
</div>
|]

postSummary :: String
postSummary = [i|
<div class="post-summary">
|]

closePostSummary :: String
closePostSummary = [i|
</div>
</div>
</div>
|]