{-# LANGUAGE OverloadedStrings #-}

module Parsers.PreferenceTags where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Control.Monad (void)
import Ast.AbstractSyntaxTree

import Parsers.Paragraphs

-- Função auxiliar para ignorar espaços, quebras de linha e tabulações
space' :: Parser ()
space' = void $ many (char ' ' <|> char '\n' <|> char '\t')

parsePreferenceTag :: Parser [Preferences]
parsePreferenceTag = do
    _ <- string "(preferences)"
    space'
    prefs <- manyTill parsePreferenceContent (string "(/preferences)")
    space'
    return prefs

parsePreferenceContent :: Parser Preferences
parsePreferenceContent =
      parseLanguage
  <|> parsePageTag
  <|> parseContentTag
  <|> parseSummaryTag
  <|> parseFigureListTag
  <|> parseReferencesTag

parseLanguage :: Parser Preferences
parseLanguage = do
    _ <- string "(language)"
    space'
    content <- manyTill anySingle (string "(/language)")
    space'
    return (Language content)

parsePageTag :: Parser Preferences
parsePageTag = do
    _ <- string "(page)"
    space'
    elems <- manyTill parsePageElement (string "(/page)")
    space'
    return (Page elems)

parsePageElement :: Parser PageElement
parsePageElement = parsePageSize <|> parsePageNumberSize <|> parsePageMargin

parsePageSize :: Parser PageElement
parsePageSize = do
    _ <- string "(size)"
    space'
    content <- manyTill anySingle (string "(/size)")
    space'
    return (PageSize content)

parsePageNumberSize :: Parser PageElement
parsePageNumberSize = do
    _ <- string "(page-number-size)"
    space'
    content <- manyTill anySingle (string "(/page-number-size)")
    space'
    return (PageNumberSize content)

parsePageMargin :: Parser PageElement
parsePageMargin = do
    _ <- string "(margin)"
    space'
    margins <- manyTill parseMargin (string "(/margin)")
    space'
    return (PageMargin margins)

parseMargin :: Parser Margin
parseMargin =
      parseMarginTop
  <|> parseMarginBottom
  <|> parseMarginLeft
  <|> parseMarginRight

parseMarginTop, parseMarginBottom, parseMarginLeft, parseMarginRight :: Parser Margin
parseMarginTop = do
    _ <- string "(top)"
    space'
    val <- manyTill anySingle (string "(/top)")
    space'
    return (MarginTop val)

parseMarginBottom = do
    _ <- string "(bottom)"
    space'
    val <- manyTill anySingle (string "(/bottom)")
    space'
    return (MarginBottom val)

parseMarginLeft = do
    _ <- string "(left)"
    space'
    val <- manyTill anySingle (string "(/left)")
    space'
    return (MarginLeft val)

parseMarginRight = do
    _ <- string "(right)"
    space'
    val <- manyTill anySingle (string "(/right)")
    space'
    return (MarginRight val)

parseContentTag :: Parser Preferences
parseContentTag = do
    _ <- string "(content)"
    space'
    cs <- manyTill parseContentItem (string "(/content)")
    space'
    return (Content cs)

parseContentItem :: Parser Content
parseContentItem = do
  c <-  parseFontArial
    <|> parseFontTimes
    <|> parseFontOther
    <|> parseTitleSize
    <|> parseChapSize
    <|> parseTextSize
    <|> parseLineHeight
    <|> parseImageSize
    <|> parseBoldSectionTitles
  space'
  return c

parseFontArial = string "(font-arial)" >> return (FontArial True)
parseFontTimes = string "(font-times)" >> return (FontTimes True)
parseBoldSectionTitles = string "(bold-section-titles)" >> return (BoldSectionTitles True)

parseFontOther = do
    _ <- string "(font-other)"
    content <- manyTill anySingle (string "(/font-other)")
    return (FontOther content)

parseTitleSize = do
    _ <- string "(title-size)"
    content <- manyTill anySingle (string "(/title-size)")
    space'
    return (TitleSize content)

parseChapSize = do
    _ <- string "(chap-size)"
    content <- manyTill anySingle (string "(/chap-size)")
    space'
    return (ChapSize content)

parseTextSize = do
    _ <- string "(text-size)"
    content <- manyTill anySingle (string "(/text-size)")
    space'
    return (TextSize content)

parseLineHeight = do
    _ <- string "(line-height)"
    content <- manyTill anySingle (string "(/line-height)")
    space'
    return (LineHeight content)

parseImageSize = do
    _ <- string "(image-size)"
    content <- manyTill anySingle (string "(/image-size)")
    space'
    return (ImageSize content)

parseSummaryTag :: Parser Preferences
parseSummaryTag = do
    _ <- string "(summary)"
    space'
    s <- manyTill parseSummaryPreferences (string "(/summary)")
    space'
    return (SummaryPrefs s)

parseSummaryPreferences :: Parser SummaryPreferences
parseSummaryPreferences =
      (string "(title-align-center)" >> space' >> return (SummaryTitleAlignCenter True))
  <|> (string "(title-align-left)"   >> space' >> return (SummaryTitleAlignLeft True))
  <|> (string "(title-bold)"         >> space' >> return (SummaryTitleBold True))
  <|> (string "(chap-bold)"          >> space' >> return (SummaryChapBold True))
  <|> (string "(bold-number)"        >> space' >> return (SummaryBoldNumber True))
  <|> (string "(bold-whole-number)"  >> space' >> return (SummaryBoldWholeNumber True))
  <|> do
        _ <- string "(title-size)"
        val <- manyTill anySingle (string "(/title-size)")
        space'
        return (SummaryTitleSize val)

parseFigureListTag :: Parser Preferences
parseFigureListTag = do
    _ <- string "(figures)"
    space'
    fs <- manyTill parseFigureListPreferences (string "(/figures)")
    space'
    return (FiguresPrefs fs)

parseFigureListPreferences :: Parser FigureListPreferences
parseFigureListPreferences =
      (string "(title-align-center)" >> space' >> return (FiguresTitleAlignCenter True))
  <|> (string "(title-align-left)"   >> space' >> return (FiguresTitleAlignLeft True))
  <|> (string "(title-bold)"         >> space' >> return (FiguresTitleBold True))
  <|> (string "(chap-bold)"          >> space' >> return (FiguresChapBold True))
  <|> (string "(bold-number)"        >> space' >> return (FiguresBoldNumber True))
  <|> do
        _ <- string "(title-size)"
        val <- manyTill anySingle (string "(/title-size)")
        space'
        return (FiguresTitleSize val)

parseReferencesTag :: Parser Preferences
parseReferencesTag = do
    _ <- string "(references)"
    space'
    rs <- manyTill parseReferencesPreferences (string "(/references)")
    space'
    return (ReferencesPrefs rs)

parseReferencesPreferences :: Parser ReferencesPreferences
parseReferencesPreferences =
    string "(order-alphabetic)" >> space' >> return (Alphabetic True)