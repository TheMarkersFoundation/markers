{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Markers.Converter.Abnt where

import Control.Monad.State (State, get, put, runState)
import Data.List (intercalate)
import Data.String.Interpolate.IsString (i)
import qualified Data.Map.Strict as M
import Language.Markers.Ast.Content (Content(..))
import Language.Markers.Ast.Text (Writing(..))
import Language.Markers.Ast.Tree (Markers(..), Meta(..), Preferences(..), Section(..))
import Language.Markers.Ast.Types (File(..))
import Data.Char (toLower, isAlphaNum, toUpper)

type Counters = [Int]
type RenderState = (Counters, Int)
type SummaryEntry = (String, Int, String, String)
type FigureEntry = (Int, String, String)
type RefKey = (String, String, String, String, String)
type ReferenceEntry = (Int, RefKey)
type ReferenceIndex = M.Map RefKey Int

toAbnt :: Markers -> String
toAbnt (Document (Preference title metas) sections) =
  let contents = concatMap (\(Section _ cs) -> cs) sections
      (preSections, postSections) = splitSections contents
      tocEntries = collectChapters postSections
      figureEntries = collectFigures contents
      referenceEntries = collectReferences contents
      referenceIndex = M.fromList [(k, n) | (n, k) <- referenceEntries]
      (preHtml, stateAfterPre) =
        runState (renderContents tocEntries figureEntries referenceIndex referenceEntries preSections) ([], 0)
      (postHtml, _) =
        runState (renderContents tocEntries figureEntries referenceIndex referenceEntries postSections) stateAfterPre
  in [i|<!DOCTYPE html>
<html lang="pt-BR">
<head>
  <meta charset="UTF-8">
  <title>#{escapeHtml title}</title>
  <style>
#{abntStyle}
  </style>
  #{abntPagedPreview}
</head>
<body>
  <div class="container">
    <div class="pre-summary">
      #{renderCoverPages title metas}
      #{preHtml}
    </div>
    <div class="post-summary">
      #{postHtml}
    </div>
  </div>
  <div id="global-footer" style="display:none">
    #{renderFooter metas}
  </div>
  #{abntScript}
</body>
</html>
|]

renderContents :: [SummaryEntry] -> [FigureEntry] -> ReferenceIndex -> [ReferenceEntry] -> [Content] -> State RenderState String
renderContents tocEntries figureEntries referenceIndex referenceEntries =
  fmap concat . mapM (contentToAbnt tocEntries figureEntries referenceIndex referenceEntries)

contentToAbnt :: [SummaryEntry] -> [FigureEntry] -> ReferenceIndex -> [ReferenceEntry] -> Content -> State RenderState String
contentToAbnt _ _ referenceIndex _ (Paragraph writings) =
  pure [i|<p class="indent">#{writingsToHtml referenceIndex writings}</p>|]
contentToAbnt _ _ referenceIndex _ (Url file writings) =
  pure (fileToHtml file (writingsToHtml referenceIndex writings))
contentToAbnt tocEntries figureEntries referenceIndex referenceEntries (Chapter level chapterTitle chapterContents) = do
  incrementCounter level
  numbering <- getNumbering level
  let ident = slugify (numbering ++ "-" ++ chapterTitle)
  childrenHtml <- renderContents tocEntries figureEntries referenceIndex referenceEntries chapterContents
  let headingTag = headingTagName level
  pure [i|
<div class="chapter level-#{level}">
  <#{headingTag} id="#{ident}">
    <span class="chapter-number">#{numbering}</span> #{escapeHtml chapterTitle}
  </#{headingTag}>
  #{childrenHtml}
</div>
|]
contentToAbnt tocEntries figureEntries referenceIndex referenceEntries (ArrowList level listTitle contents) = do
  childrenHtml <- renderContents tocEntries figureEntries referenceIndex referenceEntries contents
  pure [i|
<details class="arrow-list level-#{level}">
  <summary>#{escapeHtml listTitle}</summary>
  #{childrenHtml}
</details>
|]
contentToAbnt _ _ _ _ (CodeBlock code) =
  pure [i|<pre class="abnt-code"><code>#{escapeHtml code}</code></pre>|]
contentToAbnt _ _ _ _ (Table headers rows) =
  pure (tableToHtml headers rows)
contentToAbnt _ _ referenceIndex _ (Footer page writings) =
  pure [i|<div class="abnt-footer-note-source" data-page="#{show page}">#{writingsToHtml referenceIndex writings}</div>|]
contentToAbnt _ _ referenceIndex _ (Figure (Image filePath) caption source) = do
  figureNumber <- incrementFigureCounter
  let descHtml = writingsToHtml referenceIndex caption
      descText = if null (trimText descHtml) then "SEM DESCRICAO" else descHtml
      sourceHtml = writingsToHtml referenceIndex source
      figureId = "fig-" ++ show figureNumber
  pure [i|
<figure id="#{figureId}" class="abnt-figure">
  <figcaption class="figure-title">FIGURA #{show figureNumber} - #{descText}</figcaption>
  <img src="#{escapeHtml filePath}" alt="">
  <figcaption class="figure-source">Fonte: #{sourceHtml}</figcaption>
</figure>
|]
contentToAbnt _ _ referenceIndex _ (Figure file caption source) =
  pure (fileToHtml file (writingsToHtml referenceIndex caption ++ " " ++ writingsToHtml referenceIndex source))
contentToAbnt tocEntries figureEntries referenceIndex referenceEntries (BulletList _ items) = do
  itemsHtml <- mapM (bulletItemToHtml tocEntries figureEntries referenceIndex referenceEntries) items
  pure [i|
<ul class="abnt-list">
  #{concat itemsHtml}
</ul>
|]
contentToAbnt _ _ _ _ Break =
  pure "<div class=\"separator\"></div>"
contentToAbnt tocEntries _ _ _ Summary =
  pure (summaryToHtml tocEntries)
contentToAbnt _ figureEntries _ _ FigureList =
  pure (figureListToHtml figureEntries)
contentToAbnt _ _ _ referenceEntries References =
  pure (referencesToHtml referenceEntries)

bulletItemToHtml :: [SummaryEntry] -> [FigureEntry] -> ReferenceIndex -> [ReferenceEntry] -> Content -> State RenderState String
bulletItemToHtml _ _ referenceIndex _ (Paragraph writings) =
  pure [i|<li>#{writingsToHtml referenceIndex writings}</li>|]
bulletItemToHtml tocEntries figureEntries referenceIndex referenceEntries content = do
  inner <- contentToAbnt tocEntries figureEntries referenceIndex referenceEntries content
  pure [i|<li>#{inner}</li>|]

fileToHtml :: File -> String -> String
fileToHtml (Image filePath) caption =
  let captionHtml :: String
      captionHtml =
        if null caption
          then ""
          else [i|<figcaption class="figure-source">#{caption}</figcaption>|]
  in [i|
<figure class="abnt-figure">
  <img src="#{escapeHtml filePath}" alt="">
  #{captionHtml}
</figure>
|]
fileToHtml (Video filePath) caption =
  [i|<p class="indent">Video: <a href="#{escapeHtml filePath}">#{escapeHtml filePath}</a> #{caption}</p>|]
fileToHtml (Audio filePath) caption =
  [i|<p class="indent">Audio: <a href="#{escapeHtml filePath}">#{escapeHtml filePath}</a> #{caption}</p>|]

headingTagName :: Int -> String
headingTagName level = "h" ++ show (min 6 (max 2 (level + 1)))

splitSections :: [Content] -> ([Content], [Content])
splitSections [] = ([], [])
splitSections xs =
  let (pre, post) = break isSummary xs
  in case post of
       [] -> (pre, [])
       (s:ss) -> (pre ++ [s], ss)

isSummary :: Content -> Bool
isSummary Summary = True
isSummary _ = False

summaryToHtml :: [SummaryEntry] -> String
summaryToHtml entries =
  let rows = concatMap summaryEntryToHtml entries
  in [i|
<div id="summary" class="summary">
  <h2 class="summary-title">SUMARIO</h2>
  <ul>
    #{rows}
  </ul>
</div>
|]

summaryEntryToHtml :: SummaryEntry -> String
summaryEntryToHtml (numbering, level, chapterTitle, ident) =
  let indentPx = max 0 ((level - 1) * 24)
      href = "#" ++ escapeHtml ident
  in "<li style=\"padding-left: " ++ show indentPx ++ "px;\">"
     ++ "<a class=\"summary-link\" href=\"" ++ href ++ "\">"
     ++ "<span class=\"summary-number\">" ++ numbering ++ "</span> "
     ++ "<span class=\"summary-text\">" ++ escapeHtml chapterTitle ++ "</span></a>"
     ++ "<span class=\"dots\"></span>"
     ++ "<span class=\"summary-page\" data-target=\"" ++ href ++ "\"></span></li>"

figureListToHtml :: [FigureEntry] -> String
figureListToHtml entries =
  let rows = concatMap figureEntryToHtml entries
  in [i|
<div id="figurelist" class="summary figure-list">
  <h2 class="summary-title">LISTA DE FIGURAS</h2>
  <ul>
    #{rows}
  </ul>
</div>
|]

figureEntryToHtml :: FigureEntry -> String
figureEntryToHtml (numbering, description, figureId) =
  let href = "#" ++ escapeHtml figureId
      descriptionUpper = map toUpper description
  in "<li>"
     ++ "<a class=\"summary-link\" href=\"" ++ href ++ "\">"
     ++ "<span class=\"summary-number\">FIGURA " ++ show numbering ++ "</span> "
     ++ "<span class=\"summary-text\">" ++ escapeHtml descriptionUpper ++ "</span></a>"
     ++ "<span class=\"dots\"></span>"
     ++ "<span class=\"summary-page figure-list-page\" data-target=\"" ++ href ++ "\"></span></li>"

referencesToHtml :: [ReferenceEntry] -> String
referencesToHtml entries =
  let rows = concatMap referenceEntryToHtml entries
  in [i|
<div id="references" class="references">
  <h2 class="summary-title">REFERENCIAS</h2>
  <ol class="references-list">
    #{rows}
  </ol>
</div>
|]

referenceEntryToHtml :: ReferenceEntry -> String
referenceEntryToHtml (n, (url, author, title, year, access)) =
  [i|<li id="reference-#{show n}">#{escapeHtml author}. <strong>#{escapeHtml title}</strong>. #{escapeHtml year}. Disponivel em: <a href="#{escapeHtml url}" target="_blank" rel="noopener noreferrer">#{escapeHtml url}</a>. Acesso em: #{escapeHtml access}.</li>|]

collectChapters :: [Content] -> [SummaryEntry]
collectChapters contents = fst (collect [] contents)
  where
    collect :: Counters -> [Content] -> ([SummaryEntry], Counters)
    collect counters [] = ([], counters)
    collect counters (Paragraph _ : rest) = collect counters rest
    collect counters (Url _ _ : rest) = collect counters rest
    collect counters (Figure _ _ _ : rest) = collect counters rest
    collect counters (Footer _ _ : rest) = collect counters rest
    collect counters (FigureList : rest) = collect counters rest
    collect counters (References : rest) = collect counters rest
    collect counters (ArrowList _ _ children : rest) =
      let (childEntries, countersAfterChildren) = collect counters children
          (restEntries, countersAfterRest) = collect countersAfterChildren rest
      in (childEntries ++ restEntries, countersAfterRest)
    collect counters (Chapter level chapterTitle children : rest) =
      let (newCounters, numbering) = incrementOnCounters counters level
          ident = slugify (numbering ++ "-" ++ chapterTitle)
          entry = (numbering, level, chapterTitle, ident)
          (childEntries, _) = collect newCounters children
          (restEntries, countersAfterRest) = collect newCounters rest
      in (entry : (childEntries ++ restEntries), countersAfterRest)
    collect counters (_:rest) = collect counters rest

collectFigures :: [Content] -> [FigureEntry]
collectFigures contents = fst (go 1 contents)
  where
    go :: Int -> [Content] -> ([FigureEntry], Int)
    go n [] = ([], n)
    go n (Figure (Image _) caption _ : rest) =
      let desc = trimText (writingsToText caption)
          figureId = "fig-" ++ show n
          entry = (n, if null desc then "SEM DESCRICAO" else desc, figureId)
          (restEntries, nextN) = go (n + 1) rest
      in (entry : restEntries, nextN)
    go n (Chapter _ _ children : rest) =
      let (childEntries, nextN) = go n children
          (restEntries, finalN) = go nextN rest
      in (childEntries ++ restEntries, finalN)
    go n (ArrowList _ _ children : rest) =
      let (childEntries, nextN) = go n children
          (restEntries, finalN) = go nextN rest
      in (childEntries ++ restEntries, finalN)
    go n (BulletList _ items : rest) =
      let (itemEntries, nextN) = go n items
          (restEntries, finalN) = go nextN rest
      in (itemEntries ++ restEntries, finalN)
    go n (_ : rest) = go n rest

collectReferences :: [Content] -> [ReferenceEntry]
collectReferences contents =
  let (_, entries) = collectFromContents M.empty [] contents
  in entries
  where
    collectFromContents :: ReferenceIndex -> [ReferenceEntry] -> [Content] -> (ReferenceIndex, [ReferenceEntry])
    collectFromContents index entries [] = (index, entries)
    collectFromContents index entries (c:cs) =
      let (indexAfterContent, entriesAfterContent) = collectFromContent index entries c
      in collectFromContents indexAfterContent entriesAfterContent cs

    collectFromContent :: ReferenceIndex -> [ReferenceEntry] -> Content -> (ReferenceIndex, [ReferenceEntry])
    collectFromContent index entries (Paragraph ws) = collectFromWritings index entries ws
    collectFromContent index entries (Figure _ caption source) =
      let (indexAfterCaption, entriesAfterCaption) = collectFromWritings index entries caption
      in collectFromWritings indexAfterCaption entriesAfterCaption source
    collectFromContent index entries (Url _ ws) = collectFromWritings index entries ws
    collectFromContent index entries (Footer _ ws) = collectFromWritings index entries ws
    collectFromContent index entries (Chapter _ _ children) = collectFromContents index entries children
    collectFromContent index entries (ArrowList _ _ children) = collectFromContents index entries children
    collectFromContent index entries (BulletList _ items) = collectFromContents index entries items
    collectFromContent index entries _ = (index, entries)

    collectFromWritings :: ReferenceIndex -> [ReferenceEntry] -> [Writing] -> (ReferenceIndex, [ReferenceEntry])
    collectFromWritings index entries [] = (index, entries)
    collectFromWritings index entries (w:ws) =
      let (indexAfterWriting, entriesAfterWriting) = collectFromWriting index entries w
      in collectFromWritings indexAfterWriting entriesAfterWriting ws

    collectFromWriting :: ReferenceIndex -> [ReferenceEntry] -> Writing -> (ReferenceIndex, [ReferenceEntry])
    collectFromWriting index entries (Bold ws) = collectFromWritings index entries ws
    collectFromWriting index entries (Italic ws) = collectFromWritings index entries ws
    collectFromWriting index entries (Underline ws) = collectFromWritings index entries ws
    collectFromWriting index entries (Strikethrough ws) = collectFromWritings index entries ws
    collectFromWriting index entries (Monospaced ws) = collectFromWritings index entries ws
    collectFromWriting index entries (Link ws _) = collectFromWritings index entries ws
    collectFromWriting index entries (Colored ws _) = collectFromWritings index entries ws
    collectFromWriting index entries (Highlighted ws _) = collectFromWritings index entries ws
    collectFromWriting index entries (Reference ws url author title year access) =
      let (indexAfterLabel, entriesAfterLabel) = collectFromWritings index entries ws
          key = normalizeRefKey (url, author, title, year, access)
      in case M.lookup key indexAfterLabel of
          Just _ -> (indexAfterLabel, entriesAfterLabel)
          Nothing ->
            let nextNumber = length entriesAfterLabel + 1
                nextIndex = M.insert key nextNumber indexAfterLabel
            in (nextIndex, entriesAfterLabel ++ [(nextNumber, key)])
    collectFromWriting index entries _ = (index, entries)

incrementOnCounters :: Counters -> Int -> (Counters, String)
incrementOnCounters counts level =
  let index = level - 1
      countLength = length counts
      extended = if countLength <= index then counts ++ replicate (index - countLength + 1) 0 else counts
      updated =
        [ if idx == index
            then (extended !! idx) + 1
            else if idx > index
              then 0
              else extended !! idx
        | idx <- [0 .. length extended - 1]
        ]
      numbering = intercalate "." (map show (take level updated))
  in (updated, numbering)

incrementCounter :: Int -> State RenderState ()
incrementCounter level = do
  (counters, figureCounter) <- get
  let index = level - 1
      countLength = length counters
      extended = if countLength <= index then counters ++ replicate (index - countLength + 1) 0 else counters
      updated =
        [ if idx == index
            then (extended !! idx) + 1
            else if idx > index
              then 0
              else extended !! idx
        | idx <- [0 .. length extended - 1]
        ]
  put (updated, figureCounter)

getNumbering :: Int -> State RenderState String
getNumbering level = do
  (counters, _) <- get
  pure (intercalate "." (map show (take level counters)))

incrementFigureCounter :: State RenderState Int
incrementFigureCounter = do
  (counters, figureCounter) <- get
  let next = figureCounter + 1
  put (counters, next)
  pure next

renderCoverPages :: String -> [Meta] -> String
renderCoverPages title metas =
  let institutionText = firstInstitution metas
      authorLines = allAuthors metas
      subtitleText = firstSubtitle metas
      descriptionText = firstDescription metas
      mentorText = firstMentor metas
      locationText = firstLocation metas
      dateText = firstDate metas
      firstPageTop = renderInstitution institutionText
      firstPageBottom = renderLocation locationText <> renderDateOrCurrent dateText
      secondPageBody = renderDescription descriptionText <> renderMentor mentorText
      secondPageBottom = renderLocation locationText <> renderDateOrCurrent dateText
  in [i|
<div class="abnt">
  <div class="cover-page cover-page-main">
    <div class="cover-top">
      #{firstPageTop}
    </div>
    <div class="cover-center">
      <div class="cover-main-author">
        #{renderAuthors authorLines}
      </div>
      <h1 class="cover-title">#{escapeHtml title}</h1>
      #{renderSubtitle subtitleText}
    </div>
    <div class="cover-bottom">
      #{firstPageBottom}
    </div>
  </div>
  <div class="cover-page cover-page-secondary">
    <div class="cover-top">
      #{renderAuthors authorLines}
    </div>
    <div class="cover-center">
      <h1 class="cover-title">#{escapeHtml title}</h1>
      #{renderSubtitle subtitleText}
    </div>
    <div class="cover-right">
      #{secondPageBody}
    </div>
    <div class="cover-bottom">
      #{secondPageBottom}
    </div>
  </div>
</div>
|]

allAuthors :: [Meta] -> [String]
allAuthors metas = concat [authors | Author authors <- metas]

firstInstitution :: [Meta] -> Maybe String
firstInstitution metas =
  case [institution | Institution institution <- metas] of
    [] -> Nothing
    (x:_) -> Just x

firstSubtitle :: [Meta] -> Maybe String
firstSubtitle metas =
  case [subtitle | Subtitle subtitle <- metas] of
    [] -> Nothing
    (x:_) -> Just x

firstDescription :: [Meta] -> Maybe String
firstDescription metas =
  case [description | Description description <- metas] of
    [] -> Nothing
    (x:_) -> Just x

firstMentor :: [Meta] -> Maybe String
firstMentor metas =
  case [mentor | Mentor mentor <- metas] of
    [] -> Nothing
    (x:_) -> Just x

firstLocation :: [Meta] -> Maybe String
firstLocation metas =
  case [location | Location location <- metas] of
    [] -> Nothing
    (x:_) -> Just x

firstDate :: [Meta] -> Maybe String
firstDate metas =
  case [dateText | Date dateText <- metas] of
    [] -> Nothing
    (x:_) -> Just x

renderInstitution :: Maybe String -> String
renderInstitution Nothing = ""
renderInstitution (Just institutionText) =
  [i|<p class="institution"><strong>#{escapeHtmlWithBreaks institutionText}</strong></p>|]

renderAuthors :: [String] -> String
renderAuthors [] = ""
renderAuthors authors =
  [i|<p class="author">#{intercalate ", " (map escapeHtmlWithBreaks authors)}</p>|]

renderSubtitle :: Maybe String -> String
renderSubtitle Nothing = ""
renderSubtitle (Just subtitleText) =
  [i|<p class="subtitle">#{escapeHtmlWithBreaks subtitleText}</p>|]

renderDescription :: Maybe String -> String
renderDescription Nothing = ""
renderDescription (Just descriptionText) =
  [i|<p class="description">#{escapeHtmlWithBreaks descriptionText}</p>|]

renderMentor :: Maybe String -> String
renderMentor Nothing = ""
renderMentor (Just mentorText) =
  [i|<p class="mentor"><strong>Orientador:</strong> #{escapeHtmlWithBreaks mentorText}</p>|]

renderLocation :: Maybe String -> String
renderLocation Nothing = ""
renderLocation (Just locationText) =
  [i|<p class="location">#{escapeHtmlWithBreaks locationText}</p>|]

renderDate :: Maybe String -> String
renderDate Nothing = ""
renderDate (Just dateText) =
  [i|<p class="date">#{escapeHtmlWithBreaks dateText}</p>|]

renderDateOrCurrent :: Maybe String -> String
renderDateOrCurrent (Just dateText) = renderDate (Just dateText)
renderDateOrCurrent Nothing = "<p class=\"date auto-year\"></p>"

renderFooter :: [Meta] -> String
renderFooter metas =
  let locationText = firstLocation metas
      dateText = firstDate metas
      left = maybe "" escapeHtmlWithBreaks locationText
      right = maybe "" escapeHtmlWithBreaks dateText
  in [i|<div class="global-footer-content"><span class="footer-left">#{left}</span><span class="footer-right">#{right}</span></div>|]

abntScript :: String
abntScript = [i|
<script>
  document.addEventListener('DOMContentLoaded', function () {
    var currentYear = String(new Date().getFullYear());
    document.querySelectorAll('.auto-year').forEach(function (element) {
      if (!element.textContent.trim()) {
        element.textContent = currentYear;
      }
    });

    function targetIdFromDataTarget(dataTarget) {
      if (!dataTarget) return '';
      return dataTarget.charAt(0) === '#' ? dataTarget.slice(1) : dataTarget;
    }

    function pageNumberFromElement(page, fallbackIndex) {
      var raw = page.getAttribute('data-page-number') || '';
      var trimmed = raw.trim();
      if (trimmed) return trimmed;
      return String(fallbackIndex + 1);
    }

    function updateSummaryPageNumbers() {
      var pages = Array.from(document.querySelectorAll('.pagedjs_page'));
      if (pages.length === 0) return false;

      var entries = Array.from(document.querySelectorAll('.summary-page'));
      if (entries.length === 0) return true;
      var resolved = 0;

      entries.forEach(function(entry) {
        var targetId = targetIdFromDataTarget(entry.getAttribute('data-target'));
        if (!targetId) return;

        var found = '';
        for (var i = 0; i < pages.length; i += 1) {
          var page = pages[i];
          if (page.querySelector('[id="' + targetId + '"]')) {
            found = pageNumberFromElement(page, i);
            break;
          }
        }

        if (found) {
          entry.textContent = found;
          resolved += 1;
        }
      });

      return resolved === entries.length;
    }

    function applyFooterNotes() {
      var pages = Array.from(document.querySelectorAll('.pagedjs_page'));
      if (pages.length === 0) return false;

      document.querySelectorAll('.abnt-page-footnotes').forEach(function (node) {
        node.remove();
      });

      var noteSources = Array.from(document.querySelectorAll('.abnt-footer-note-source'));
      if (noteSources.length === 0) return true;

      function findPageByNumber(pageNumber) {
        for (var i = 0; i < pages.length; i += 1) {
          var pageNo = parseInt(pageNumberFromElement(pages[i], i), 10);
          if (Number.isFinite(pageNo) && pageNo === pageNumber) return pages[i];
        }
        return null;
      }

      var noteCounter = 1;
      var unresolved = 0;
      noteSources.forEach(function (source) {
        var pageRaw = (source.getAttribute('data-page') || '').trim();
        var pageNumber = parseInt(pageRaw, 10);
        if (!Number.isFinite(pageNumber) || pageNumber < 1) {
          unresolved += 1;
          return;
        }

        var targetPage = findPageByNumber(pageNumber);
        if (!targetPage) {
          unresolved += 1;
          return;
        }
        var footerBox = targetPage.querySelector('.abnt-page-footnotes');
        if (!footerBox) {
          footerBox = document.createElement('div');
          footerBox.className = 'abnt-page-footnotes';
          targetPage.appendChild(footerBox);
        }

        var noteItem = document.createElement('div');
        noteItem.className = 'abnt-page-footer-note';

        var num = document.createElement('span');
        num.className = 'abnt-footnote-n';
        num.textContent = String(noteCounter);

        var txt = document.createElement('span');
        txt.className = 'abnt-footnote-t';
        txt.innerHTML = source.innerHTML;

        noteItem.appendChild(num);
        noteItem.appendChild(txt);
        footerBox.appendChild(noteItem);
        noteCounter += 1;
      });

      return unresolved === 0;
    }

    var checkerAttempts = 0;
    var checker = setInterval(function(){
      checkerAttempts += 1;
      var summaryDone = updateSummaryPageNumbers();
      var footerDone = applyFooterNotes();
      if ((summaryDone && footerDone) || checkerAttempts > 60) {
        clearInterval(checker);
      }
    }, 500);

    document.addEventListener('markers:paged-ready', function () {
      setTimeout(function () {
        updateSummaryPageNumbers();
        applyFooterNotes();
      }, 120);
    });

    window.addEventListener('resize', function(){
      setTimeout(function () {
        updateSummaryPageNumbers();
        applyFooterNotes();
      }, 300);
    });
  });
</script>
|]

abntPagedPreview :: String
abntPagedPreview = [i|
<script>
  window.PagedConfig = {
    auto: true,
    after: function () {
      document.documentElement.classList.add('paged-ready');
      document.dispatchEvent(new Event('markers:paged-ready'));
    }
  };
</script>
<script src="./js/polyfill.js"
        onerror="(function(){var s=document.createElement('script');s.src='https://unpkg.com/pagedjs/dist/paged.polyfill.js';document.head.appendChild(s);}());"></script>
|]

writingToHtml :: ReferenceIndex -> Writing -> String
writingToHtml _ (Plain text) = escapeHtml text
writingToHtml referenceIndex (Bold ws) = [i|<strong>#{writingsToHtml referenceIndex ws}</strong>|]
writingToHtml referenceIndex (Italic ws) = [i|<em>#{writingsToHtml referenceIndex ws}</em>|]
writingToHtml referenceIndex (Underline ws) = [i|<span style="text-decoration:underline">#{writingsToHtml referenceIndex ws}</span>|]
writingToHtml referenceIndex (Strikethrough ws) = [i|<span style="text-decoration:line-through">#{writingsToHtml referenceIndex ws}</span>|]
writingToHtml referenceIndex (Monospaced ws) = [i|<code>#{writingsToHtml referenceIndex ws}</code>|]
writingToHtml referenceIndex (Link ws url) = [i|<a href="#{escapeHtml url}" target="_blank" rel="noopener noreferrer">#{writingsToHtml referenceIndex ws}</a>|]
writingToHtml referenceIndex (Reference ws url author title year access) =
  let key = normalizeRefKey (url, author, title, year, access)
      labelHtml = writingsToHtml referenceIndex ws
  in case M.lookup key referenceIndex of
      Nothing ->
        [i|#{labelHtml}<sup class="reference-cite">[?]</sup>|]
      Just n ->
        [i|#{labelHtml}<sup class="reference-cite"><a href="#reference-#{show n}">[#{show n}]</a></sup>|]
writingToHtml referenceIndex (Colored ws (r, g, b)) = [i|<span style="color: rgb(#{r}, #{g}, #{b});">#{writingsToHtml referenceIndex ws}</span>|]
writingToHtml referenceIndex (Highlighted ws (r, g, b)) = [i|<span style="background-color: rgb(#{r}, #{g}, #{b});">#{writingsToHtml referenceIndex ws}</span>|]

writingsToHtml :: ReferenceIndex -> [Writing] -> String
writingsToHtml referenceIndex = concatMap (writingToHtml referenceIndex)

writingsToText :: [Writing] -> String
writingsToText = concatMap writingToText

writingToText :: Writing -> String
writingToText (Plain text) = text
writingToText (Bold ws) = writingsToText ws
writingToText (Italic ws) = writingsToText ws
writingToText (Underline ws) = writingsToText ws
writingToText (Strikethrough ws) = writingsToText ws
writingToText (Monospaced ws) = writingsToText ws
writingToText (Link ws _) = writingsToText ws
writingToText (Reference ws _ _ _ _ _) = writingsToText ws
writingToText (Colored ws _) = writingsToText ws
writingToText (Highlighted ws _) = writingsToText ws

tableToHtml :: [String] -> [[String]] -> String
tableToHtml headers rows =
  let ths = concatMap (\cell -> "<th>" ++ escapeHtml cell ++ "</th>") headers
      trs = concatMap rowToHtml rows
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
  where
    rowToHtml row =
      let tds = concatMap (\cell -> "<td>" ++ escapeHtml cell ++ "</td>") row
      in "<tr>" ++ tds ++ "</tr>"

escapeHtml :: String -> String
escapeHtml = concatMap escapeChar

escapeHtmlWithBreaks :: String -> String
escapeHtmlWithBreaks [] = ""
escapeHtmlWithBreaks ('\\':'n':xs) = "<br>" ++ escapeHtmlWithBreaks xs
escapeHtmlWithBreaks ('\\':'r':'\\':'n':xs) = "<br>" ++ escapeHtmlWithBreaks xs
escapeHtmlWithBreaks ('\n':xs) = "<br>" ++ escapeHtmlWithBreaks xs
escapeHtmlWithBreaks ('\r':'\n':xs) = "<br>" ++ escapeHtmlWithBreaks xs
escapeHtmlWithBreaks (x:xs) = escapeChar x ++ escapeHtmlWithBreaks xs

trimText :: String -> String
trimText = f . f
  where
    f = reverse . dropWhile (`elem` [' ', '\t', '\n', '\r'])

normalizeRefKey :: RefKey -> RefKey
normalizeRefKey (url, author, title, year, access) =
  (trimText url, trimText author, trimText title, trimText year, trimText access)

slugify :: String -> String
slugify s =
  let lowered = map toLower s
      replaced = map (\b -> if b == ' ' then '-' else b) lowered
  in filter (\b -> isAlphaNum b || b == '-') replaced

escapeChar :: Char -> String
escapeChar '&' = "&amp;"
escapeChar '<' = "&lt;"
escapeChar '>' = "&gt;"
escapeChar '"' = "&quot;"
escapeChar '\'' = "&#39;"
escapeChar c = [c]

abntStyle :: String
abntStyle = [i|
    @page {
      size: A4;
      margin: 3cm 2cm 2cm 3cm;
      @top-right {
        content: counter(page, decimal);
        font-family: "Times New Roman", Times, serif;
        font-size: 12pt;
      }
    }

    @page noheader {
      @top-right { content: ""; }
    }

    .pre-summary { page: noheader; }

    body {
      font-family: "Times New Roman", Times, serif;
      font-size: 12pt;
      line-height: 1.5;
      color: #000;
      background-color: #fff;
      margin: 0;
      padding: 0;
    }

    @media print {
      body {
        font-size: 12pt !important;
        -webkit-text-size-adjust: 100%;
        text-size-adjust: 100%;
      }

      p,
      li,
      summary {
        font-size: 12pt;
      }

      .cover-page {
        height: 247mm;
        min-height: 247mm;
      }
    }

    @media screen {
      html,
      body {
        min-height: 100%;
        background: #e7e7e7;
      }

      body {
        font-size: 12pt;
        -webkit-text-size-adjust: 100%;
        text-size-adjust: 100%;
        padding: 12mm 0 16mm;
      }

      body .container {
        width: 210mm;
        min-height: 297mm;
        margin: 0 auto;
        padding: 3cm 3cm 2cm 2cm;
        box-sizing: border-box;
        background: #fff;
        border: 1px solid #d3d3d3;
        box-shadow: 0 14px 34px rgba(0, 0, 0, 0.14);
      }

      .cover-page,
      .summary {
        margin-bottom: 14mm;
      }

      .separator {
        display: block;
        height: 14mm;
      }

      html.paged-ready body {
        padding-top: 10mm;
        padding-bottom: 16mm;
        display: flex !important;
        justify-content: center !important;
        align-items: flex-start !important;
      }

      html.paged-ready body .container {
        width: auto;
        min-height: 0;
        margin: 0;
        padding: 0;
        background: transparent;
        border: 0;
        box-shadow: none;
      }

      html.paged-ready .pagedjs_pages {
        display: flex !important;
        flex-direction: column;
        align-items: center !important;
        gap: 14mm !important;
        width: fit-content !important;
        max-width: 100% !important;
        margin: 0 auto !important;
        padding: 0 !important;
      }

      html.paged-ready body > .pagedjs_pages {
        margin-left: auto !important;
        margin-right: auto !important;
      }

      html.paged-ready .pagedjs_page {
        position: relative;
        margin: 0 auto !important;
        background: #fff;
        border: 1px solid #d3d3d3;
        box-shadow: 0 16px 36px rgba(0, 0, 0, 0.16);
        overflow: hidden;
      }

      html.paged-ready .pagedjs_page:not(:last-child)::after {
        content: "";
        position: absolute;
        left: 20mm;
        right: 20mm;
        bottom: -8mm;
        border-bottom: 2px dashed #b7b7b7;
      }

    }

    .abnt-footer-note-source {
      display: none !important;
    }

    html.paged-ready .abnt-page-footnotes {
      position: absolute;
      left: 30mm;
      right: 20mm;
      bottom: 10mm;
      font-size: 10pt;
      line-height: 1.1;
      text-align: justify;
      z-index: 30;
    }

    html.paged-ready .abnt-page-footnotes::before {
      content: "";
      display: block;
      width: 4cm;
      border-top: 1px solid #000;
      margin-bottom: 2mm;
    }

    html.paged-ready .abnt-page-footer-note {
      display: flex;
      gap: 0.35em;
      margin: 0 0 1.4mm 0;
    }

    html.paged-ready .abnt-footnote-n {
      min-width: 1.1em;
      font-size: 10pt;
      line-height: 1.1;
      text-align: left;
    }

    html.paged-ready .abnt-footnote-t {
      flex: 1;
      font-size: 10pt;
      line-height: 1.1;
      text-align: justify;
    }

    .container {
      width: 100%;
      margin: 0;
      padding: 0;
    }

    .abnt {
      margin-bottom: 1em;
    }

    .cover-page {
      height: 247mm;
      min-height: 247mm;
      display: flex;
      flex-direction: column;
      page-break-after: always;
      break-after: page;
    }

    .cover-page-main {
      position: relative;
    }

    .cover-page-main .cover-top {
      position: absolute;
      top: 0;
      left: 0;
      right: 0;
    }

    .cover-page-main .cover-center {
      position: absolute;
      top: 50%;
      left: 0;
      right: 0;
      transform: translateY(-50%);
      margin: 0;
    }

    .cover-main-author {
      position: relative;
      top: -0.8cm;
      margin-bottom: 2cm;
    }

    .cover-main-author:empty {
      display: none;
      margin-bottom: 0;
    }

    .cover-main-author .author {
      margin: 0;
    }

    .cover-page-main .cover-bottom {
      position: absolute;
      left: 0;
      right: 0;
      bottom: 0;
      margin: 0;
    }

    .cover-top {
      text-align: center;
      margin-top: 0;
    }

    .cover-center {
      text-align: center;
      margin-top: 2cm;
      margin-bottom: auto;
    }

    .cover-right {
      width: 60%;
      margin-left: auto;
      text-align: justify;
      margin-bottom: 2cm;
    }

    .cover-bottom {
      text-align: center;
      margin-top: auto;
      margin-bottom: 0;
    }

    .cover-bottom p {
      margin-bottom: 0;
    }

    .cover-title {
      font-size: 14pt;
      margin: 0;
      text-transform: uppercase;
    }

    .subtitle {
      margin-top: 0.6em;
    }

    .description,
    .mentor {
      margin: 0.5em 0;
      text-indent: 0;
    }

    .institution,
    .author,
    .location,
    .date {
      text-indent: 0;
      text-align: center;
    }

    .summary {
      margin-bottom: 1em;
      page-break-after: always;
      break-after: page;
    }

    .summary-title {
      text-align: center;
      font-size: 14pt;
      font-weight: bold;
      margin-bottom: 1em;
      text-transform: uppercase;
    }

    .summary ul {
      list-style: none;
      margin: 0;
      padding: 0;
    }

    .summary li {
      display: flex;
      align-items: baseline;
      gap: 0.2em;
      margin: 0.2em 0;
      white-space: nowrap;
    }

    .summary-number {
      font-weight: bold;
    }

    .summary-text {
      text-transform: uppercase;
    }

    .summary-link {
      color: #000;
      text-decoration: none;
    }
    .summary-link:visited,
    .summary-link:hover,
    .summary-link:active {
      color: #000;
      text-decoration: none;
    }

    .summary .dots {
      flex: 1;
      border-bottom: 1px dotted #000;
      margin-top: 0.2em;
      margin-right: 0.1em;
    }

    .summary .summary-page {
      margin-left: 0.1em;
      min-width: 1.8em;
      text-align: right;
    }

    .reference-cite {
      font-size: 0.75em;
      vertical-align: super;
      line-height: 0;
    }

    .reference-cite a {
      color: #000;
      text-decoration: none;
    }

    h2,
    h3,
    h4,
    h5,
    h6 {
      font-size: 14pt;
      margin: 1.5em 0 0.5em 0;
      text-align: left;
      text-transform: uppercase;
    }

    .chapter-number {
      font-weight: bold;
    }

    p {
      margin: 0 0 1em 0;
      text-align: justify;
    }

    .indent {
      text-indent: 1.25cm;
    }

    .abnt-list {
      margin: 0 0 1em 0;
      padding-left: 1.25cm;
    }

    .abnt-list li {
      margin: 0.4em 0;
      text-align: justify;
    }

    .abnt-code {
      font-family: "Courier New", Courier, monospace;
      font-size: 10pt;
      line-height: 1.25;
      white-space: pre-wrap;
      margin: 1em 0;
      text-indent: 0;
    }

    .abnt-figure {
      text-align: center;
      margin: 1em 0;
      break-inside: avoid;
      page-break-inside: avoid;
    }

    .abnt-figure img {
      max-width: 100%;
      height: auto;
    }

    table {
      margin: 1em auto;
      border-collapse: collapse;
      border: 1px solid #000;
      width: 90%;
      max-width: 100%;
      text-align: center;
    }

    th,
    td {
      border: 1px solid #000;
      padding: 0.35em 0.5em;
      text-align: center;
      vertical-align: middle;
    }

    th {
      font-weight: bold;
    }

    .figure-source {
      font-size: 10pt;
      font-style: italic;
      margin-top: 0.5em;
      text-indent: 0;
      text-align: center;
    }

    .references {
      margin-top: 1.5em;
      page-break-before: always;
      break-before: page;
    }

    .references-list {
      margin: 0;
      padding-left: 1.5em;
    }

    .references-list li {
      margin: 0.45em 0;
      text-align: left;
      line-height: 1.2;
    }

    .arrow-list {
      margin: 0 0 1em 0;
    }

    .arrow-list > summary {
      font-weight: bold;
      cursor: pointer;
      text-transform: uppercase;
    }

    .separator {
      page-break-before: always;
      break-before: page;
    }
|]
