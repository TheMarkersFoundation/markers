module Parsers.AllTags where

-- This module contains a function that has every tag
-- Present in the Markers Markup Language. This is currently
-- Needed for the parsing of Plain text.

-- As per not make it too voodoo, this will also be used as
-- an internal documentation.

allTags :: [String]
allTags = ["(c)", "(/c)"
          ,"(u)", "(/u)"
          ,"(u)", "(/u)"
          ,"(b)", "(/b)"
          ,"(i)", "(/i)"
          ,"(k)", "(/k)"
          ,"(i)", "(/i)"
          ,"(sm)", "(/sm)"
          ,"(tp)", "(/tp)"
          ,"(color |", "(/color)"
          ,"(chap |", "(/chap)"
          ,"(>> |", "(/>>)"
          ,"(code)", "(/code)"
          ,"(nl)", "(/nl)"
          ,"(bl)", "(/bl)"
          ,"(ll)", "(/ll)"
          ,"(align-center)", "(/align-center)"
          ,"(align-right)", "(/align-right)"
          ,"(ref |", "(/ref)"
          ,"(meta)", "(/meta)"
          ,"(link |", "(/link)"
          ,"(localimg |", "(/localimg)"
          ,"(img |", "(/img)"
          ,"(video |", "(/video)"
          ,"(audio |", "(/audio)"
          ,"(table)", "(/table)"
          ,"(quote |", "(/quote)"
          ,"(--", "--)"
          ,"(math)", "(/math)"
          ,"(math |", "(/math)"
          ,"(thanks)", "(/thanks)"
          ,"(abstract |", "(/abstract)"
          ,"(abbreviations |", "(/abbreviations)"
          ,"(figurelist)", "(mathlist)"
          ,"(summary)", "(preferences)"
          ,"(/preferences)"
          ,"(hr)", "(br)"
          ,"(references)", "(references |", "\r\n", "\n"]