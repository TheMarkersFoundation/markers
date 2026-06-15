module Language.Markers.Math (resolveDocumentMath) where

import qualified Data.Map.Strict as M
import Language.Markers.Ast.Content (Content (..))
import Language.Markers.Ast.Text (Writing (..))
import Language.Euler.Ast.Block (Stmt (..))
import Language.Euler.Env (Env)
import Language.Euler.Parser (parseEuler)
import Language.Euler.Converter.Html (renderEuler, renderInline)

{-

    The math resolution pass. It runs once over a document's contents and:

      * collects every definition across all math (block and inline) into a
        single, document-wide environment, so a name may be referenced anywhere
        after (or before) it is defined; and
      * rewrites the tree, rendering each math fragment with that environment.

    Block math (Math) keeps its Math node but its payload becomes the rendered
    inner HTML; the converters add the surrounding equation wrapper/number.
    Inline math (MathInline) becomes a Raw node holding finished HTML.

-}

resolveDocumentMath :: [Content] -> [Content]
resolveDocumentMath contents =
  let env = buildEnv contents
  in map (resolveContent env) contents

-- Build the global environment -----------------------------------------------

buildEnv :: [Content] -> Env
buildEnv = M.fromList . concatMap defsContent
  where
    defsContent c = case c of
      Math src -> defsSource src
      Paragraph ws -> concatMap defsWriting ws
      Figure _ caption source -> concatMap defsWriting caption ++ concatMap defsWriting source
      Url _ ws -> concatMap defsWriting ws
      Footer _ ws -> concatMap defsWriting ws
      Quote body source -> concatMap defsWriting body ++ concatMap defsWriting source
      Chapter _ _ cs -> concatMap defsContent cs
      ArrowList _ _ cs -> concatMap defsContent cs
      BulletList _ cs -> concatMap defsContent cs
      _ -> []

    defsWriting w = case w of
      MathInline src -> defsSource src
      Bold ws -> concatMap defsWriting ws
      Italic ws -> concatMap defsWriting ws
      Underline ws -> concatMap defsWriting ws
      Strikethrough ws -> concatMap defsWriting ws
      Monospaced ws -> concatMap defsWriting ws
      Link ws _ -> concatMap defsWriting ws
      Reference ws _ _ -> concatMap defsWriting ws
      Footnote ws -> concatMap defsWriting ws
      Colored ws _ -> concatMap defsWriting ws
      Highlighted ws _ -> concatMap defsWriting ws
      _ -> []

    defsSource src = case parseEuler src of
      Right block -> [(name, e) | Define name e <- block]
      Left _ -> []

-- Rewrite the tree -----------------------------------------------------------

resolveContent :: Env -> Content -> Content
resolveContent env c = case c of
  Math src -> Math (renderEuler env src)
  Paragraph ws -> Paragraph (map (resolveWriting env) ws)
  Figure f caption source -> Figure f (map (resolveWriting env) caption) (map (resolveWriting env) source)
  Url f ws -> Url f (map (resolveWriting env) ws)
  Footer p ws -> Footer p (map (resolveWriting env) ws)
  Quote body source -> Quote (map (resolveWriting env) body) (map (resolveWriting env) source)
  Chapter l t cs -> Chapter l t (map (resolveContent env) cs)
  ArrowList l t cs -> ArrowList l t (map (resolveContent env) cs)
  BulletList l cs -> BulletList l (map (resolveContent env) cs)
  other -> other

resolveWriting :: Env -> Writing -> Writing
resolveWriting env w = case w of
  MathInline src -> Raw (renderInline env src)
  Bold ws -> Bold (map (resolveWriting env) ws)
  Italic ws -> Italic (map (resolveWriting env) ws)
  Underline ws -> Underline (map (resolveWriting env) ws)
  Strikethrough ws -> Strikethrough (map (resolveWriting env) ws)
  Monospaced ws -> Monospaced (map (resolveWriting env) ws)
  Link ws u -> Link (map (resolveWriting env) ws) u
  Reference ws t fs -> Reference (map (resolveWriting env) ws) t fs
  Footnote ws -> Footnote (map (resolveWriting env) ws)
  Colored ws col -> Colored (map (resolveWriting env) ws) col
  Highlighted ws col -> Highlighted (map (resolveWriting env) ws) col
  other -> other
