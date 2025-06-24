module Converter.Math where
import Ast.AbstractSyntaxTree
import Data.List.Split (splitOn)
import Data.List (intersperse)

import qualified Data.Map.Strict as M
import Converter.Helpers (greekMap, escapeMathSymbol)

renderMath :: MathExpr -> String
renderMath (Number n)        = n
renderMath (Add a b)         = renderBin a "+" b
renderMath (Sub a b)         = renderBin a "−" b
renderMath (Mul a b)         = renderBin a "·" b
renderMath (Div a b)         = renderBin a "÷" b

renderMath (ImplicitMul a b) = "<span class=\"implicit-mul\">" <> renderMath a <> renderMath b <> "</span>"
renderMath (Ellipsis)        = "<span class=\"ellipsis\">...</span>"
renderMath (Parens x)        = "<span class=\"paren\">(" <> renderMath x <> ")</span>"
renderMath (PowerOf a b)     = renderMath a <> "<sup>" <> renderMath b <> "</sup>"
renderMath (SquareRoot x)    = escapeMathSymbol "√" <> "<span class=\"radicand\">" <> renderMath x <> "</span>"
renderMath (Neg x)           = "<span class=\"operator\">" <> escapeMathSymbol "−" <> "</span>" <> renderMath x

renderMath (Fraction n d) =
  "<span class=\"fraction\">"
  <> "<span class=\"num\">" <> renderMath n <> "</span>"
  <> "<span class=\"den\">" <> renderMath d <> "</span>"
  <> "</span>"

renderMath (Var v) =
  let parts = splitOn "_" v 
      base  = head parts
      subs  = tail parts
      sym0  = M.findWithDefault base base greekMap
  in  foldl (\acc sub -> acc ++ "<sub>" ++ sub ++ "</sub>") sym0 subs

renderMath (Eq a b) =
  renderMath a
  <> "<span class=\"operator\"> = </span>"
  <> renderMath b

renderMath (Probability e c) =
  "<span class=\"probability\">"
  <> "<em>P</em>(" <> renderMath e
  <> "<span class=\"sep\">|</span>"
  <> renderMath c
  <> ")</span>"

renderMath (Arrow a b) =
  renderMath a
  <> "<span class=\"operator\">" <> escapeMathSymbol "→" <> "</span>"
  <> renderMath b  

renderMath (Sum i0 iN body) =
  "<span class=\"sum\">" <> escapeMathSymbol "Σ" <> "<sub>" <> renderMath i0 <> "</sub>"
  <> "<sup>" <> renderMath iN <> "</sup> " <> renderMath body <> "</span>"

renderMath (Product i0 iN body) =
  "<span class=\"prod\">" <> escapeMathSymbol "Π" <> "<sub>" <> renderMath i0 <> "</sub>"
  <> "<sup>" <> renderMath iN <> "</sup> " <> renderMath body <> "</span>"

renderMath (Integral mb body) =
  let bounds = case mb of
                Nothing      -> ""
                Just (a,b)   -> "<sub>" <> renderMath a <> "</sub>" <>
                                "<sup>" <> renderMath b <> "</sup>"
  in "<span class=\"int\">" <> escapeMathSymbol "∫" <> bounds <> " " <> renderMath body <> "</span>"

renderMath (Limit at body) =
  "<span class=\"lim\">lim<sub>" <> renderMath at <> "</sub> " <> renderMath body <> "</span>"

renderMath (Derivative d body) =
  "<span class=\"deriv\">" <> renderMath d <> " " <> renderMath body <> "</span>"

renderMath (Root mb x) =
  let prefix = case mb of
                 Nothing -> ""
                 Just n  -> "<sup>" <> renderMath n <> "</sup>"
  in prefix <> escapeMathSymbol "√" <> "<span class=\"radicand\">" <> renderMath x <> "</span>"

renderMath (Binom n k) =
  "<span class=\"binom\">" <> escapeMathSymbol "⎛" <> renderMath n <> escapeMathSymbol "⎞"
  <> escapeMathSymbol "⎝" <> renderMath k <> escapeMathSymbol "⎠" <> "</span>"

renderMath (Abs x) = "<span class=\"abs\">|" <> renderMath x <> "|</span>"

renderMath (Vector vs) =
  "<span class=\"vector\">" <> escapeMathSymbol "⟨"
  <> mconcat (intersperse ", " (map renderMath vs))
  <> escapeMathSymbol "⟩" <> "</span>"

renderMath (Matrix rows) =
  let renderRow r = "<tr>" <>
                      mconcat (map (\c -> "<td>" <> renderMath c <> "</td>") r)
                      <> "</tr>"
  in "<table class=\"matrix\">" <> mconcat (map renderRow rows) <> "</table>"

renderMath (Func f args) =
  "<span class=\"func\"><em style=\"margin-right: 10px\">" <> f <> "</em>("
  <> mconcat (intersperse ", " (map renderMath args))
  <> ")</span>"

renderMath (Piecewise cs) =
  let renderCase (e,c) = "<div class=\"case\"><span class=\"expr\">"
                        <> renderMath e <> "</span> if <span class=\"cond\">"
                        <> renderMath c <> "</span></div>"
  in "<div class=\"cases\">" <> mconcat (map renderCase cs) <> "</div>"

renderBin a op b =
  renderMath a
  <> "<span class=\"operator\">" <> escapeMathSymbol op <> "</span>"
  <> renderMath b