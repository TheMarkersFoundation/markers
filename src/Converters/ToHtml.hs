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

toHtml :: Markers -> String
toHtml (MarkersMain title prefs content) =
  let config = applyPreferences prefs
  in header title (lang config)
    <> tex 
    <> openStyle
      <> math
      <> body (backgroundColor config) (textColor config) (fontFamily config) (fontSize config) (chapterSize config) (textSize config) (lineHeight config)
      <> (if paperSize config == "A4" then container (backgroundColor config) else "")
    <> closeStyle
    <> openScript
    <> closeScript
    <> closeHeader
    <> openBody
      <> [i|<div class="markers-content-container">|]
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

      convert (Centered content) = [i|
          <div style="text-align: center;">
            #{Prelude.foldr (\x acc -> convert x <> acc) "" content}
          </div>
      |]

      convert (RightContent content) = [i|
          <div style="text-align: right;">
            #{Prelude.foldr (\x acc -> convert x <> acc) "" content}
          </div>
      |]

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

      convert (Code content) = [i|
      <pre class="abnt-code">
        #{content}
      </pre>
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

      convert (Video url content)
            = [i| <center>
                    <video src="#{url}" style="width: 60%" controls>
                      #{treatText content}
                    </video>
                  </center>
              |]

      convert (Audio url content)
            = [i| <center>
                    <audio src="#{url}" controls>
                      #{treatText content}
                    </audio>
                  </center>
              |]

      convert (Meta content) = case content of 
                [Institution c] -> "<div style=\"display: none;\" class=\"institution\">" <> Prelude.concatMap escapeHtml c <> "</div>"
                [Author c]      -> "<div style=\"display: none;\" class=\"author\">" <> c <> "</div>"
                [Subtitle c]    -> "<div style=\"display: none;\" class=\"subtitle\">" <> c <> "</div>"
                [Location c]    -> "<div style=\"display: none;\" class=\"location\">" <> c <> "</div>"
                [Year c]        -> "<div style=\"display: none;\" class=\"year\">" <> c <> "</div>"
                [Description c] -> "<div style=\"display: none;\" class=\"description\">" <> c <> "</div>"
                _               -> "<!-- META CONTENT -->"

      convert Empty = ""

      convert _ = [i|<script>console.log("A tag was not found in the parsing of this file! Please open a issue at https://github.com/TheMarkersFoundation/markers/")</script>|]