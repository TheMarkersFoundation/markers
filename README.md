# Markers: The General Purpose Academic Markup Language

<p align="center">
  <img width="30%" src="resources/img/logo.png" alt="Markers - Monographic Infrastructure for Open Research, Integrated Notation & Editing" style="padding-bottom: 2px" />
</p>

<h3 align="center">
  A simple, powerful, and accessible markup language designed for academic papers, monographs, and general-purpose document editing.
</h3>



<p align="center">
  <a href="https://miragem.app.br/">
    <img alt="Project Status" src="https://img.shields.io/badge/Status-Development%20(Pre--Alpha)-red"
  ></a>
  <a href="#features">
    <img alt="Focus" src="https://img.shields.io/badge/Focus-Academic-blue"
  ></a>
  <a href="#technology">
    <img alt="Language" src="https://img.shields.io/badge/Built%20With-Haskell-orange"
  ></a>
</p>

Markers is the next-generation, **Markdown-inspired markup language** focused on simplicity and power for academic and general uses. It converts a special flavor of **Markdown** to a variety of formats, aiming to be the **easiest and most accessible academic paper formatting language** available.

Markers is built <strong>entirely in Haskell</strong>, for robust performance and ease of use.

<br>

## Running Markers
To convert a `.Markers` file to the Brazillian ABNT format, for example, you can run the following command:

```markers --format=abnt <input-file>```

This will start a server that listens for file changes and automatically updates the output whenever the input file is modified, this way, you can edit your `Markers` file in your favorite text editor and see the results in real time on the browser!

*Note:* You'll still need to print the page from the browser to get the final PDF output, but this workflow allows you to focus on writing without worrying about formatting until you're ready to export.*

## 🚀 Key Features and Advantages

Markers is engineered for a **hassle-free** writing and formatting experience, providing the tools needed for professional-grade academic output without the complexity of traditional typesetting systems.

* **Markdown-Inspired Syntax:** Forget complex opening and closing tags. Markers adopts a simple, intuitive syntax, making it incredibly **easy to learn** and fast to write.
* **Automatic Document Structure:** Features like **automatic chapter numbering** and **summary/table of contents generation** are built-in, handling tedious formatting tasks automatically.
* **Monographic Infrastructure:** Designed specifically to handle the structure of **monographs**, **theses**, and **research papers**.
* **Built in Haskell:** The entire Markers compiler is implemented in **Haskell**, ensuring **high performance**, **reliability**, and robust **type-safety**.
* **Custom Styling Language (TISS):** Markers will feature its own custom stylesheet language (TISS), allowing for granular control over the document's appearance, either in a separate file or inline.
* **Complete Text Formatting:** Full support for **bold** (`*text*`), *italics* (`_text_`), ~~strikethrough~~ (`~text~`), and <u>underline</u> (`__text__`), along with monospaced text and **colored text** using Hexadecimal or RGB syntax.

<br>

## 📜 Creating a Markers Document

A `.Markers` document is structured for clarity and ease of use, separating essential document components: **Title**, **Metadata**, and **Content**.

### Document Structure
A Markers document always begins with the title, followed by optional metadata, and then the content.

1.  **Title:** The very first line of the document.
2.  **Metadata:** Essential academic information defined by simple keywords.
3.  **Content:** The main body of the paper, composed of text, chapters, lists, etc.

### Metadata Keywords
Metadata is defined using simple keywords:

* `By:` (Author Names)
* `At:` (Institution/Affiliation)
* `Mentor:` (Advisor/Mentor's Name)
* `Description:` (Brief description of the document)
* `Location:` (City or General Location)
* `Date:` (Publication Date)
* `Keywords:` (SEO-friendly terms, separated by commas)

### Example Document

```Markers
A Markers Document Title
By: Person A, Person B
At: University of Open Source
Location: Brazil
Keywords: Markers, Markup Language, Academic Paper, Haskell

This is the main body text, which is *not encapsulated* in any structural tag.

# This is a Chapter (Level 1 Heading)
Chapters act as sections and automatically populate the table of contents.

# This is the Next Chapter.
In the future, more tags shall be available.
## Nested Chapters (Level 2 Subheading)
Chapters are automatically closed when a new one is encountered.
```

<br>

## 🎨 Text Formatting

Markers utilizes familiar Markdown-like syntax for inline text styling:

| Syntax | Output | Description |
| :--- | :--- | :--- |
| `*This is bold*` | **This is bold** | Bold text |
| `_This is italic_` | *This is italic* | Italicized text |
| `~This is strikethrough~` | <s>This is strikethrough</s> | Strikethrough text |
| `__This is underlined__` | <u>This is underlined</u> | Underlined text |
| `` `Monospaced` `` | `Monospaced` | Code/Monospaced font |
| `{#FF0000}(Red Text)` | (The text in red) | Colored text (Hex) |
| `{0,0,255}(Blue Text)` | (The text in blue) | Colored text (RGB) |

<br>

## ➗ Euler: Markers's Math Language

Euler is Markers's own little math notation language. It is purposely far simpler
than LaTeX: a handful of ASCII symbols, named greek letters, powers, indices,
fractions, roots, matrices and accents cover most academic formulae. A **display
block** is written with the `!math( ... )` tag on its own line (it is numbered
and listed); the very same tag used **inline**, in the middle of a paragraph,
flows with the surrounding text:

```Markers
!math(E = m*c^2)

The Pythagorean theorem !math(a^2 + b^2 = c^2) appears mid-sentence.

!math(
x = (-b +- sqrt(b^2 - 4*a*c)) / (2*a)
)
```

| Euler | Renders as | Description |
| :--- | :--- | :--- |
| `x^2` | x² | Superscript / power |
| `x_1` | x₁ | Subscript / index |
| `a/b` | a fraction | Stacked fraction |
| `sqrt(x)` | √x̄ | Square root |
| `*` | · | Multiplication (also implicit, e.g. `2a`) |
| `+-` | ± | Plus-minus |
| `<=` `>=` `!=` | ≤ ≥ ≠ | Comparison operators |
| `->` `=>` `<->` `<=>` | → ⇒ ↔ ⇔ | Arrows / implications |
| `[a, b; c, d]` | a matrix / vector | Rows split by `;`, cells by `,` |
| `\|x\|`, `norm(x)` | \|x\|, ‖x‖ | Absolute value, norm |
| `floor(x)`, `ceil(x)` | ⌊x⌋, ⌈x⌉ | Floor / ceiling |
| `vec(x)` `hat(x)` `bar(x)` `dot(x)` `tilde(x)` | x⃗ x̂ x̄ ẋ x̃ | Accents |
| `...` | ⋯ | Ellipsis |
| `alpha`, `pi`, `Omega`, `inf`, ... | α, π, Ω, ∞ | Greek letters & symbols by name |

Named **definitions** use `:=` (an optional `def` keyword reads nicely). A defined
name is **substituted** wherever it is referenced afterwards - in other equations
or inline in the text - so you write the short name and the full expression is
rendered:

```Markers
!math(
def phi := (1 + sqrt(5)) / 2
)

Referencing !math(phi) later expands to its definition automatically.
```

### `::MathList::` (List of Equations)

Every `!math(...)` block becomes a numbered equation. The `::MathList::` tag
generates an automatic **LISTA DE EQUAÇÕES** (List of Equations) for ABNT
documents, exactly like `::FigureList::` does for figures. Place it where the
list should appear, typically next to `::Summary::`:

```Markers
::Summary::
::FigureList::
::MathList::
```

<br>

## 🧭 Roadmap

Markers is in active development. Our goal is to offer a comprehensive feature set essential for academic publishing and research:

* [x] Text Formatting
* [x] Chapters and Arrow-Lists
* [x] Document Metadata
* [x] HTML Export
* [x] Automatic Chapter and Summary/ToC Generation
* [x] Bullet Lists, Arrow Lists
* [-] Link, Image, Video and Audio Support
* [ ] References, Quotes, and Footnote Support
* [x] Page and Printing Support
* [ ] PDF Export
* [x] Exportação para Norma ABNT (**ABNT Norm Export** - Crucial for Brazilian academia)
* [ ] Custom Stylesheets with TISS
* [-] Euler (Markers's Simple Math Language) Implementation
