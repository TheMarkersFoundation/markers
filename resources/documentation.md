Markers Language Documentation
By: MIGUEL AGUIAR
At: CENTRO ESTADUAL DE EDUCAÇÃO TECNOLÓGICA PAULA SOUZA\nFaculdade de Tecnologia Baixada Santista Rubens Lara\nCurso Superior de Tecnologia em Análise e Desenvolvimento de Sistemas
Location: Santos - SP, Brazil
Date: 2026
Description: Trabalho de Conclusão de Curso apresentado à Faculdade de Tecnologia Rubens Lara, como exigência para a obtenção do Título de Tecnólogo em Análise e Desenvolvimento de Sistemas.
Mentor: Alexandre Garcia de Oliveira
Keywords: Markers, language, tutorial

::Summary::

# Overview
Markers is a lightweight markup language for writing readable, structured plain-text documents. It supports conversion to multiple formats and includes features tailored for technical documentation and academic papers.

Markers can be used to write:
- Technical documentation
- Academic papers
- Blog posts
- Forum posts
- Any structured text documents

The language is simple to learn, flexible to use, and released under the MIT License.

---

# Quick Example
Below is a short, modern example of a Markers document. Note how metadata appears at the top and how sections, tables and images are written.

```
My Paper Title
By: Miguel Aguiar
At: Open Source Community\nMarkers Project
Location: Brazil
Keywords: markers, example

# Introduction
This is a paragraph with a [link](https://example.com).

![Project logo](./img/logo.png)

[Column A|Column B|Column C]
[1|2|3]
[4|5|6]
```

---

# Writing Guide

## Main Structure
A Markers file should start with the title and an optional metadata block (as shown above). After metadata you can use `::Summary::` and regular sections using `#`, `##`, `###`.

## Inline Formatting
- *bold*: `*text*`
- _italic_: `_text_`
- __underline__: `__text__`
- ~strikethrough~: `~text~`
- `inline code`: use backticks
- `{#RRGGBB}(colored)` or `{r,g,b}(colored)` for colored text

Examples: This is *bold*, this is _italic_, this is __underline__, this is ~strikethrough~, and this is `code`.

## Links and Images
Use the literal syntax below:

```
[label](https://...)
![caption | Autor](path/to/image.png)
```

Local images are embedded as base64 during conversion when possible.

## References (ABNT)
A reference is written inline as `$[label](fields)`. The label becomes an auto-numbered superscript citation that links to the references list, and the entry itself is rendered, in ABNT style, wherever you place `::References::`. Identical entries are deduplicated and share the same number.

A single letter between `$` and `[` selects the reference style. Fields are separated by `|` in the fixed order shown below; optional fields (marked *) may be left empty between the pipes, but the empty slot must still be present.

```
$[label](url|author|title|year|access)                                          web (documento online)
$b[label](author|title|edition*|city|publisher|year)                            book (livro)
$a[label](author|title|journal|volume*|number*|pages*|year)                     article (artigo de periodico)
$c[label](author|chapterTitle|bookAuthor|bookTitle|city|publisher|year|pages*)  book chapter (capitulo)
$t[label](author|title|workType|institution|city|year)                          academic work (tese/dissertacao)
```

Example: `$b[Davis](DAVIS, E. Haskell|Functional Forms||Big Timber, Mont.|Seven Buffaloes Press|1987)` renders as:

DAVIS, E. Haskell. **Functional Forms**. Big Timber, Mont.: Seven Buffaloes Press, 1987.

Enter each field without trailing punctuation; the converter adds the ABNT separators. A work type such as `Dissertacao (Mestrado em Educacao)` may contain parentheses.

## Code Blocks
Use fenced code blocks with triple backticks. Everything between fences is preserved literally; use them when you want to show `---` or `___` without creating a separator.

```
// JavaScript example
const x = 10;
console.log(x);
```

Inline example: `` `---` `` will render the literal `---`.

---

# Lists
- Bullet list items start with `-`.

# Tables
Declare rows with square brackets and separate columns with `|`. The first row is the header.

[Header A|Header B|Header C]
[a1|b1|c1]
[a2|b2|c2]

---

# Metadata and Preferences
Use the top-of-file metadata block to declare authorship, location, date and keywords. These fields are used by converters (e.g., ABNT export).

# Special Tags and Blocks
Markers supports additional constructs useful for academic and technical writing: code blocks, images, tables, collapsible arrow lists, and localized color formatting.

# Contributing & Tools
- Editor recommendation: Miragem â€” provides syntax highlighting and autocompletion for Markers.
- Parser: available in the repository; builds on Windows and Linux.
- License: MIT

# Donations
If you would like to support development, donation options can be provided in this section (PIX, Ko-Fi, PicPay, etc.).
