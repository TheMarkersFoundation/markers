{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Converters.ToAbnt where
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

import Helpers.Abnt

toAbnt :: Markers -> String
toAbnt (MarkersMain title prefs content) =
  let config = applyAbntPreferences prefs
      (preSections, postSections) = splitSections content
  in header title (lang config)
    <> tex 
    <> openStyle
      <> math
      <> abntPage (paperSize config) (marginTop config) (marginBottom config) (marginLeft config) (marginRight config) (fontFamily config) (fontSize config)
      <> abntBody (backgroundColor config) (textColor config) (fontFamily config) (fontSize config) (chapterSize config) (textSize config) (lineHeight config)
      <> abntThanks (titleAlign config) (titleSize config)
      <> abntAbstract (titleAlign config) (titleSize config) "2"
      <> abbreviations (if boldSectionTitles config then "bold" else "400") (titleAlign config)
      <> abntSummary (titleAlign config) (titleSize config)
                     (if boldSectionTitles config then "bold" else "400") (if titleBold config then "bold" else "400")
      <> abntFigures (figureAlign config) (figureSize config)
                     (if boldSectionTitles config then "bold" else "400") (if figureNumberBold config then "bold" else "400")
      <> abntEquations (figureAlign config) (figureSize config)
                     (if boldSectionTitles config then "bold" else "400") (if figureNumberBold config then "bold" else "400")
      <> abntImageStyle (imageSize config)
      <> abntTables
      <> abntCode
    <> closeStyle
    <> openScript
      <> mergeParagraphs
      <> equationList
      <> summaryList (titleBold config) (boldWholeNumber config)
      <> figureList (figureNumberBold config)
      <> references (referencesAlphabetic config)
    <> closeScript
    <> closeHeader
    <> openBody
      <> preSummary
        <> Prelude.foldr (\x acc -> convert x <> acc) "" preSections
      <> closePreSummary
      <> postSummary
        <> Prelude.foldr (\x acc -> convert x <> acc) "" postSections
      <> closePostSummary
    <> closeBody
    <> end
  where
    convert :: MainSection -> String
    convert (Paragraph tags) = treatText tags
    convert (Helpy x)        = convertHelpie x (applyAbntPreferences prefs)

    convert (Summary content) = [i|
      <div id="summary" class="summary">
        <h3 class="summary-title">#{content}</h3>
      </div>
    |]

    convert (Thanks content) = [i|
      <div id="thanks" class="thanks">
        <h3 class="thanks-title">AGRADECIMENTOS</h3>
        #{Prelude.foldr (\x acc -> convert x <> acc) "" content}
      </div>
    |]

    convert (Abstract title content) = [i|
      <div id="abstract" class="abstract">
        <h3 class="abstract-title">#{title}</h3>
        #{Prelude.foldr (\x acc -> convert x <> acc) "" content}
      </div>
    |]

    convert (MathBlock expression) = [i|
      <div class="math-block">
        #{foldr (\x acc -> renderMath x <> acc) "" expression}
      </div>
    |]

    convert (MathBlockWithPage page expression) = [i|
      <div class="math-block">
        <span id="mathPageNumber" style="display: none">#{page}</span>
        #{foldr (\x acc -> renderMath x <> acc) "" expression}
      </div>
    |]

    convert (Abbreviations title content) = [i|
      <div id="abstract" class="abstract">
        <h3 class="abstract-title">#{title}</h3>
        #{Data.List.concatMap abbconvert content}
      </div>
    |]
      where
        abbconvert (Abbr name meaning) = [i|
        <div class="abbr-item">
          <span class="abbr-name">#{name}</span>
          <span class="abbr-meaning">#{meaning}</span><br>
        </div>
        |]
        abbconvert _ = ""

    convert (BulletList items) =
      let liItems = map (\item ->
                case item of
                  Paragraph tags -> [i|<li>#{treatText tags}</li>|]
              ) items
      in [i|
      <ul>
        #{mconcat liItems}
      </ul>
      |]

    convert (NumberedList items) =
        let liItems = map (\item ->
                  case item of
                    Paragraph tags -> [i|<li>#{treatText tags}</li>|]
                    _ -> ""
                ) items
        in [i|
        <ol>
          #{mconcat liItems}
        </ol>
        |]

    convert (LetteredList items) =
        let liItems = map (\item ->
                  case item of
                    Paragraph tags -> [i|<li>#{treatText tags}</li>|]
                    _ -> ""
                ) items
        in [i|
        <ol type="a">
          #{mconcat liItems}
        </ol>
        |]


    convert (Figurelist) = [i|
    <div id="figurelist" class="figurelist">
      <h3 class="summary-title">LISTA DE FIGURAS</h3>
    </div>
    |]

    convert (MathList) = [i|
    <div id="equationlist" class="equationlist">
      <h3 class="summary-title">LISTA DE EQUAÇÕES</h3>
    </div>
    |]

    convert (Chap title content) = [i|
    <div class="chapter">
      <h2 style="font-weight: bold;">#{title}</h2>
      #{Prelude.foldr (\x acc -> convert x <> acc) "" content}
    </div>
    |]

    convert (List title content) = [i|
    <div class="chapter">
      <h2 style="font-weight: bold;">#{title}</h2>
      #{Prelude.foldr (\x acc -> convert x <> acc) "" content}
    </div>
    |]

    convert (PagedChapter page title content) = [i|
    <div class="chapter">
      <span id="chapterPageNumber" style="display: none">#{page}</span>
      <h2 style="font-weight: bold;">#{title}</h2>
      #{Prelude.foldr (\x acc -> convert x <> acc) "" content}
    </div>
    |]

    convert (ImageUrl url content) = [i|
    <div class="figure-item">
      <figure style="text-align:center;">
        <p class="figure-title" style="font-size:12pt; font-style:italic;"></p>
        <img src="#{url}" alt="">
        <figcaption class="figure-source" style="font-size:10pt; font-style:italic;">
          #{treatText content}
        </figcaption>
        <span class="figure-page-number" style="display:none;">?</span>
      </figure>
    </div>
    |]

    convert (ImageUrlPage page url content) = [i|
    <div class="figure-item">
      <figure style="text-align:center;">
        <p class="figure-title" style="font-size:12pt; font-style:italic;"></p>
        <img src="#{url}" alt="">
        <figcaption class="figure-source" style="font-size:10pt; font-style:italic;">
          #{treatText content}
        </figcaption>
        <span class="figure-page-number" style="display:none;">#{page}</span>
      </figure>
    </div>
    |]

    convert (Image b64 mimeType content) = [i|
    <div class="figure-item">
      <figure style="text-align:center;">
        <p class="figure-title" style="font-size:12pt; font-style:italic;"></p>
        <img src="data:image/#{mimeType};base64,#{b64}" alt="">
        <figcaption class="figure-source" style="font-size:10pt; font-style:italic;">
          #{treatText content}
        </figcaption>
        <span class="figure-page-number" style="display:none;">?</span>
      </figure>
    </div>
    |]

    convert (ImagePage page b64 mimeType content) = [i|
    <div class="figure-item">
      <figure style="text-align:center;">
        <p class="figure-title" style="font-size:12pt; font-style:italic;"></p>
        <img src="data:image/#{mimeType};base64,#{b64}" alt="" />
        <figcaption class="figure-source" style="font-size:10pt; font-style:italic;">
          #{treatText content}
        </figcaption>
        <span class="figure-page-number" style="display:none;">#{page}</span>
      </figure>
    </div>
    |]

    convert (Code content) = [i|
      <pre class="abnt-code">
        #{content}
      </pre>
    |]

    convert (Quote author content) =
      let quoteText = treatText content
      in [i|<blockquote class="abnt-quote"><p>#{quoteText} #{author}</p></blockquote>|]

    convert (Ref url author title year access content) = [i|
    <p class="indent">
      <span class="reference">
        <span style="visibility: none; display: none" class="title">#{title}</span>
        <span style="visibility: none; display: none" class="author">#{author}</span>
        <span style="visibility: none; display: none" class="year">#{year}</span>
        <span style="visibility: none; display: none" class="access">#{access}</span>
        <span style="visibility: none; display: none" class="url">#{url}</span>
        <span class="content" style="display:inline;">
          #{Prelude.foldr (\x acc -> convert x <> acc) "" content}
        </span>
      </span>
    </p>
    |]

    convert (Link url content) = [i|
    <a href="#{url}">
      #{treatText content}
    </a>
    |]

    convert (Commentary content) = [i|<!-- #{content} -->|]

    convert References = [i|
    <div class="references">
      <span style="visibility: none; display: none" id="referencesPage"></span>
      <h2 class="summary-title">REFERÊNCIAS</h2>
      <div class="references-list"></div>
    </div>
    |]

    convert (ReferencesPaged s) = [i|
    <div class="references">
      <span style="visibility: none; display: none" id="referencesPage">#{s}</span>
      <h2 class="summary-title">REFERÊNCIAS</h2>
      <div class="references-list"></div>
    </div>
    |]

    convert (Table headers rows) =
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

    convert (Meta content) =
      let topAbnt       = Prelude.foldr (\x acc -> convertTopAbnt x <> acc) "" content
          bottomAbnt    = Prelude.foldr (\x acc -> convertBottomAbnt x <> acc) "" content
          secondTop     = Prelude.foldr (\x acc -> convertSecondPageAbntTop x <> acc) "" content
          secondBottom  = Prelude.foldr (\x acc -> convertSecondPageAbntBottom x <> acc) "" content
          secondFooter  = Prelude.foldr (\x acc -> convertSecondPageAbntFooter x <> acc) "" content
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

    convert Empty = ""

    convert _ = [i|<script>console.log("A tag was not found in the parsing of this file! Please open a issue at https://github.com/TheMarkersFoundation/markers/")</script>|]

    convertTopAbnt :: MetaSection -> String
    convertTopAbnt (Institution c) = [i|
    <div style="text-align: center;">
      <p class="institution" style="margin-bottom: 30%">
        <b>#{Prelude.concatMap escapeHtml c}</b>
      </p>
    </div>
    |]

    convertTopAbnt (Author c) = [i|
    <div style="text-align: center;">
      <p class="author" style="margin-bottom: 30%">#{c}</p>
    </div>
    |]

    convertTopAbnt _ = ""

    convertBottomAbnt :: MetaSection -> String
    convertBottomAbnt (Subtitle c) = [i|
    <div style="text-align: center;">
      <p class="subtitle" style="margin-top: -15px; margin-bottom: 55%">#{c}</p>
    </div>
    |]

    convertBottomAbnt (Location c) = [i|
    <div style="text-align: center;">
      <p class="location">#{c}</p>
    </div>
    |]

    convertBottomAbnt (Year c) = [i|
    <div style="text-align: center;">
      <p class="year" style="margin-bottom: 80px">#{c}</p>
    </div>
    |]

    convertBottomAbnt _ = ""

    convertSecondPageAbntTop :: MetaSection -> String
    convertSecondPageAbntTop (Author c) = [i|
    <div style="text-align: center;">
      <p class="author" style="margin-bottom: 30%">#{c}</p>
    </div>
    |]

    convertSecondPageAbntTop _ = ""

    convertSecondPageAbntBottom :: MetaSection -> String
    convertSecondPageAbntBottom (Subtitle c) = [i|
    <div style="text-align: center;">
      <p class="subtitle" style="margin-top: -15px; margin-bottom: 35%">#{c}</p>
    </div>
    |]

    convertSecondPageAbntBottom (Description c) = [i|
    <div style="margin-top: 0px; text-align: justify; margin-right: 0;">
      <p class="description" style="width: 60%; float: right;">#{c}</p>
    </div>
    |]

    convertSecondPageAbntBottom (Mentor m) = [i|
    <div style="margin-top: 0px; text-align: justify; margin-right: 0;">
      <p class="mentor" style="width: 60%; float: right;">
        <br><br><strong>Orientador:</strong> #{m}
      </p>
    </div>
    |]

    convertSecondPageAbntBottom _ = ""

    convertSecondPageAbntFooter :: MetaSection -> String
    convertSecondPageAbntFooter (Location c) = [i|
    <div style="clear: both; text-align: center;">
      <p class="location">#{c}</p>
    </div>
    |]

    convertSecondPageAbntFooter (Year c) = [i|
    <div style="clear: both; text-align: center;">
      <p class="year" style="margin-bottom: 80px">#{c}</p>
    </div>
    |]

    convertSecondPageAbntFooter _ = ""