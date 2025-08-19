{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Converters.ToHtml where
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


abntBody :: String -> String -> String -> String -> String -> String -> String -> String
abntBody bgcolor textcolor font titleSize chapterTitleSize textSize lineHeight = [i| 
    body {
        font-family: #{font};
        font-size: #{textSize}pt;
        line-height: #{lineHeight};
        text-align: justify;
        background-color: #{bgcolor};
        color: #{textcolor};
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

toHtml :: Markers -> String
toHtml (MarkersMain title prefs content) =
  let config = applyPreferences prefs
  in header title (lang config)
    <> tex 
    <> openStyle
      <> math
      <> body (backgroundColor config) (textColor config) (fontFamily config) (fontSize config) (chapterSize config) (textSize config) (lineHeight config)
      <> (if paperSize config == "A4" then (container) else "")
    <> closeStyle
    <> openScript
    <> closeScript
    <> closeHeader
    <> openBody
      <> [i|<div class="container">|]
      <> [i|<h1>#{title}</h1>|]
      <> Prelude.foldr (\x acc -> convert x <> acc) "" content
      <> [i|</div>|]
    <> closeBody
    <> end
    where
      convert :: MainSection -> String
      convert (Paragraph tags) = treatText tags
      convert (Helpy x)        = convertHelpie x (applyAbntPreferences prefs)

      convert (Commentary content) = [i|<!--#{content}-->|]  

      convert (Link url content) = [i|<a class="link" href="#{url}">#{treatText content}</a>|]

      convert (Chap title content)
        = [i|
          <div class="chapter">
            <h2>#{title}</h2>
            #{Prelude.foldr (\x acc -> convert x <> acc) "" content}
          </div>
          |]

      convert (PagedChapter _ title content)
        = [i|
          <div class="chapter">
            <h2>#{title}</h2>
            #{Prelude.foldr (\x acc -> convert x <> acc) "" content}
          </div>
          |]

      convert (List title content)
        = [i|
          <details class="arrow-list">
            <summary>#{title}</summary>
              <div>#{Prelude.foldr (\x acc -> convert x <> acc) "" content}</div>
          </details>
          |]

      convert (Quote author content)
        = [i|
          <blockquote class="block-quote">
              <p>#{treatText content}</p>
              <footer>#{author}</footer>
          </blockquote>
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

      convert (Table headers rows) =
        let
          ths = concatMap (\x -> [i|<th>#{x}</th>|]) headers
          trs = concatMap
                  (\row ->
                    let tds = concatMap (\cell -> [i|<td>#{cell}</td>|]) row
                    in [i|<tr>#{tds}</tr>|]
                  ) rows
        in [i|
          <table class="table-style">
            <thead>
              <tr>#{ths}</tr>
            </thead>
            <tbody>
              #{trs}
            </tbody>
          </table>
        |]

      convert (NumberedList items) = 
        let liItems = concatMap (\secs -> [i|<li>#{concatMap convert secs}</li>|]) items
        in [i|
        <ol>
            #{liItems}
        </ol>
        |]

      convert (BulletList items) = 
        let liItems = concatMap (\secs -> [i|<li>#{concatMap convert secs}</li>|]) items
        in [i|
        <ul>
          #{liItems}
        </ul>
        |]

      convert (LetteredList items) = 
        let liItems = concatMap (\secs -> [i|<li>#{concatMap convert secs}</li>|]) items
        in [i|
        <ol type="a">
          #{liItems}
        </ol>
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

      convert _ = "<b>[ UNSUPPORTED TAG, PLEASE OPEN A ISSUE AT https://github.com/TheMarkersFoundation/markers ]</b>"