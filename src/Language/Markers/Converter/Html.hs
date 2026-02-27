{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.Markers.Converter.Html where

import Control.Monad.State (State, evalState, get, gets, put)
import Data.List (intercalate)
import qualified Data.Map.Strict as M
import Data.String.Interpolate.IsString (i)
import Language.Markers.Ast.Content (Content(..))
import Language.Markers.Ast.Text (Writing(..))
import Language.Markers.Ast.Tree (Markers(..), Preferences(..), Section(..))
import Language.Markers.Ast.Types (File(..))

type Counters = [Int]
type SummaryEntry = (String, Int, String)
type RefKey = (String, String, String, String, String)
type ReferenceEntry = (Int, RefKey)
type ReferenceIndex = M.Map RefKey Int

toHtml :: Markers -> String
toHtml (Document (Preference title _) sections) =
  evalState
    (do
       sectionsHtml <- mapM sectionToHtml sections
       pure [i|<html>
  <head>
    <title>#{title}</title>
    <style>
      .reference-cite { font-size: 0.75em; vertical-align: super; line-height: 0; }
      .reference-cite a { text-decoration: none; color: inherit; }
      .references { margin-top: 1.5em; }
      .references ol { margin: 0; padding-left: 1.5em; }
      .references li { margin: 0.35em 0; }
    </style>
  </head>
  <body>
    <h1>#{title}</h1>
    #{concat sectionsHtml}
  </body>
</html>|])
    []

sectionToHtml :: Section -> State Counters String
sectionToHtml (Section secTitle contents) = do
  let tocEntries = collectChapters contents
      referenceEntries = collectReferences contents
      referenceIndex = M.fromList [(k, n) | (n, k) <- referenceEntries]
      contentToHtml' = contentToHtml tocEntries referenceIndex referenceEntries
  contentsHtml <- mapM contentToHtml' contents
  pure [i|
    <section class="#{secTitle}">
      #{concat contentsHtml}
    </section>
  |]

contentToHtml :: [SummaryEntry] -> ReferenceIndex -> [ReferenceEntry] -> Content -> State Counters String
contentToHtml _ referenceIndex _ (Paragraph writings) =
  pure [i|<p>#{writingsToHtml referenceIndex writings}</p>|]
contentToHtml _ referenceIndex _ (Url f ws) =
  pure (mediaToHtml referenceIndex f ws)
contentToHtml _ referenceIndex _ (Figure f caption source) =
  pure (figureToHtml referenceIndex f caption source)
contentToHtml _ referenceIndex _ (Table headers rows) =
  pure (tableToHtml headers rows)
contentToHtml _ referenceIndex _ (Footer page writings) =
  pure [i|<aside class="footer-note" data-page="#{show page}">#{writingsToHtml referenceIndex writings}</aside>|]
contentToHtml toc referenceIndex referenceEntries (Chapter level chTitle chContents) =
  blockToHtmlWithToc toc referenceIndex referenceEntries "chapter" level chTitle chContents
contentToHtml toc referenceIndex referenceEntries (ArrowList level title contents) =
  arrowListHtmlWithToc toc referenceIndex referenceEntries "arrow-list" level title contents
contentToHtml toc _ _ Summary =
  pure (tocHtml toc)
contentToHtml _ _ _ FigureList =
  pure "<div class=\"figure-list\"><h2>LISTA DE FIGURAS</h2></div>"
contentToHtml _ _ referenceEntries References =
  pure (referencesToHtml referenceEntries)
contentToHtml _ _ _ (CodeBlock code) =
  pure [i|<pre><code>#{escapeHtml code}</code></pre>|]
contentToHtml toc referenceIndex referenceEntries (BulletList level items) =
  bulletListHtml toc referenceIndex referenceEntries level items
contentToHtml _ _ _ Break =
  pure "<br/>"

writingsToHtml :: ReferenceIndex -> [Writing] -> String
writingsToHtml referenceIndex = concatMap (writingToHtml referenceIndex)

writingToHtml :: ReferenceIndex -> Writing -> String
writingToHtml _ (Plain text) = text
writingToHtml referenceIndex (Bold ws) = [i|<strong>#{writingsToHtml referenceIndex ws}</strong>|]
writingToHtml referenceIndex (Italic ws) = [i|<em>#{writingsToHtml referenceIndex ws}</em>|]
writingToHtml referenceIndex (Underline ws) = [i|<u>#{writingsToHtml referenceIndex ws}</u>|]
writingToHtml referenceIndex (Strikethrough ws) = [i|<s>#{writingsToHtml referenceIndex ws}</s>|]
writingToHtml referenceIndex (Monospaced ws) = [i|<code>#{writingsToHtml referenceIndex ws}</code>|]
writingToHtml referenceIndex (Link ws url) =
  [i|<a href="#{escapeHtml url}" target="_blank" rel="noopener noreferrer">#{writingsToHtml referenceIndex ws}</a>|]
writingToHtml referenceIndex (Reference ws url author title year access) =
  let key = normalizeRefKey (url, author, title, year, access)
      labelHtml = writingsToHtml referenceIndex ws
  in case M.lookup key referenceIndex of
      Nothing ->
        [i|#{labelHtml}<sup class="reference-cite">[?]</sup>|]
      Just n ->
        [i|#{labelHtml}<sup class="reference-cite"><a href="#reference-#{show n}">[#{show n}]</a></sup>|]
writingToHtml referenceIndex (Colored ws (r, g, b)) =
  [i|<span style="color: rgb(#{r}, #{g}, #{b});">#{writingsToHtml referenceIndex ws}</span>|]
writingToHtml referenceIndex (Highlighted ws (r, g, b)) =
  [i|<span style="background-color: rgb(#{r}, #{g}, #{b});">#{writingsToHtml referenceIndex ws}</span>|]

mediaToHtml :: ReferenceIndex -> File -> [Writing] -> String
mediaToHtml referenceIndex (Image path) caption =
  let captionText = writingsToHtml referenceIndex caption
  in [i|<figure><img src="#{escapeHtml path}" alt=""><figcaption>#{captionText}</figcaption></figure>|]
mediaToHtml referenceIndex (Video path) caption =
  let captionText = writingsToHtml referenceIndex caption
  in [i|<p>Video: <a href="#{escapeHtml path}" target="_blank" rel="noopener noreferrer">#{escapeHtml path}</a> #{captionText}</p>|]
mediaToHtml referenceIndex (Audio path) caption =
  let captionText = writingsToHtml referenceIndex caption
  in [i|<p>Audio: <a href="#{escapeHtml path}" target="_blank" rel="noopener noreferrer">#{escapeHtml path}</a> #{captionText}</p>|]

figureToHtml :: ReferenceIndex -> File -> [Writing] -> [Writing] -> String
figureToHtml referenceIndex (Image path) caption source =
  let captionText = writingsToHtml referenceIndex caption
      sourceText = writingsToHtml referenceIndex source
      sourceHtml = if null (trim sourceText) then "" else "<figcaption>Fonte: " ++ sourceText ++ "</figcaption>"
  in [i|<figure><img src="#{escapeHtml path}" alt=""><figcaption>#{captionText}</figcaption>#{sourceHtml}</figure>|]
figureToHtml referenceIndex file caption _ =
  mediaToHtml referenceIndex file caption

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

referencesToHtml :: [ReferenceEntry] -> String
referencesToHtml entries =
  let rows = concatMap referenceEntryToHtml entries
  in [i|
    <div id="references" class="references">
      <h2>REFERENCIAS</h2>
      <ol>
        #{rows}
      </ol>
    </div>
  |]

referenceEntryToHtml :: ReferenceEntry -> String
referenceEntryToHtml (n, (url, author, title, year, access)) =
  [i|<li id="reference-#{show n}">#{escapeHtml author}. <strong>#{escapeHtml title}</strong>. #{escapeHtml year}. Disponivel em: <a href="#{escapeHtml url}" target="_blank" rel="noopener noreferrer">#{escapeHtml url}</a>. Acesso em: #{escapeHtml access}.</li>|]

headingTagName :: Int -> String
headingTagName level = "h" ++ show (min 6 (max 2 (level + 1)))

blockToHtmlWithToc :: [SummaryEntry] -> ReferenceIndex -> [ReferenceEntry] -> String -> Int -> String -> [Content] -> State Counters String
blockToHtmlWithToc toc referenceIndex referenceEntries className level title contents = do
  let headingTag = headingTagName level
  incrementCounter level
  numbering <- getNumbering level
  let contentToHtml' = contentToHtml toc referenceIndex referenceEntries
  contentsHtml <- mapM contentToHtml' contents
  pure [i|
    <div class="#{className} level-#{level}">
      <#{headingTag}>#{numbering} #{title}</#{headingTag}>
      #{concat contentsHtml}
    </div>
  |]

arrowListHtmlWithToc :: [SummaryEntry] -> ReferenceIndex -> [ReferenceEntry] -> String -> Int -> String -> [Content] -> State Counters String
arrowListHtmlWithToc toc referenceIndex referenceEntries className level title contents = do
  let contentToHtml' = contentToHtml toc referenceIndex referenceEntries
  contentsHtml <- mapM contentToHtml' contents
  pure [i|
    <details class="#{className} level-#{level}">
      <summary>#{title}</summary>
      #{concat contentsHtml}
    </details>
  |]

bulletListHtml :: [SummaryEntry] -> ReferenceIndex -> [ReferenceEntry] -> Int -> [Content] -> State Counters String
bulletListHtml toc referenceIndex referenceEntries level items = do
  let contentToHtml' = contentToHtml toc referenceIndex referenceEntries
  itemsHtml <- mapM
    (\b -> case b of
        Paragraph ws ->
          pure ("<li>" ++ writingsToHtml referenceIndex ws ++ "</li>")
        _ -> do
          inner <- contentToHtml' b
          pure ("<li>" ++ inner ++ "</li>"))
    items
  pure [i|<ul class="bullet-list level-#{level}">
    #{concat itemsHtml}
  </ul>|]

tocHtml :: [SummaryEntry] -> String
tocHtml entries =
  let rows = concatMap (\(num, _, t) -> "<p>" ++ num ++ " " ++ t ++ "</p>\n") entries
  in "<div class=\"toc\">\n" ++ rows ++ "</div>"

collectChapters :: [Content] -> [SummaryEntry]
collectChapters contents = fst (collect [] contents)
  where
    collect :: Counters -> [Content] -> ([SummaryEntry], Counters)
    collect counters [] = ([], counters)
    collect counters (Paragraph _ : rest) = collect counters rest
    collect counters (Url _ _ : rest) = collect counters rest
    collect counters (Figure _ _ _ : rest) = collect counters rest
    collect counters (References : rest) = collect counters rest
    collect counters (ArrowList _ _ children : rest) =
      let (childEntries, countersAfterChildren) = collect counters children
          (restEntries, countersAfterRest) = collect countersAfterChildren rest
      in (childEntries ++ restEntries, countersAfterRest)
    collect counters (Chapter lvl title children : rest) =
      let (newCounters, numbering) = incrementOnCounters counters lvl
          entry = (numbering, lvl, title)
          (childEntries, countersAfterChildren) = collect newCounters children
          (restEntries, countersAfterRest) = collect newCounters rest
      in (entry : (childEntries ++ restEntries), countersAfterRest)
    collect counters (_:rest) = collect counters rest

    incrementOnCounters :: Counters -> Int -> (Counters, String)
    incrementOnCounters counts level =
      let idx = level - 1
          len = length counts
          countsExt = if len <= idx then counts ++ replicate (idx - len + 1) 0 else counts
          updated =
            [ if i == idx then (countsExt !! i) + 1
              else if i > idx then 0
              else countsExt !! i
            | i <- [0 .. length countsExt - 1]
            ]
          numbering = intercalate "." (map show (take level updated))
      in (updated, numbering)

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

normalizeRefKey :: RefKey -> RefKey
normalizeRefKey (url, author, title, year, access) =
  (trim url, trim author, trim title, trim year, trim access)

incrementCounter :: Int -> State Counters ()
incrementCounter level = do
  counters <- get
  let idx = level - 1
      len = length counters
      countersExt = if len <= idx then counters ++ replicate (idx - len + 1) 0 else counters
      updated =
        [ if i == idx then (countersExt !! i) + 1
          else if i > idx then 0
          else countersExt !! i
        | i <- [0 .. length countersExt - 1]
        ]
  put updated

getNumbering :: Int -> State Counters String
getNumbering level = do
  counters <- gets (take level)
  pure (intercalate "." (map show counters))

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile (`elem` [' ', '\t', '\n', '\r'])

escapeHtml :: String -> String
escapeHtml = concatMap escapeChar
  where
    escapeChar '&' = "&amp;"
    escapeChar '<' = "&lt;"
    escapeChar '>' = "&gt;"
    escapeChar '"' = "&quot;"
    escapeChar '\'' = "&#39;"
    escapeChar c = [c]
