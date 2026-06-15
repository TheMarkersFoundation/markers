Reference Styles Test
By: Test Author
At: Test Institution
Location: Santos - SP, Brazil
Date: 2026
Description: Documento de teste para os estilos de referencia ABNT.
Keywords: abnt, references, test

::Summary::

---

# Referencias

Website citation $[site](https://hackage.haskell.org/package/megaparsec|Mark Karpov|Megaparsec: Monadic parser combinators|2024|27 Feb. 2026).

Book citation, no edition (empty slot) $b[davis](DAVIS, E. Haskell|Functional Forms||Big Timber, Mont.|Seven Buffaloes Press|1987).

Book with edition $b[livro](SOBRENOME, Nome|Titulo do Livro|2. ed|Sao Paulo|Editora Exemplo|2020).

Journal article $a[artigo](AUTOR, Nome|Titulo do artigo|Revista Brasileira de Exemplos|10|2|45-60|2019).

Book chapter $c[capitulo](AUTOR, Cap|Titulo do capitulo|ORG, Livro|Titulo do Livro|Rio de Janeiro|Editora ABC|2018|120-140).

Academic work $t[tese](FONSECA, J. J. S.|Metodologia da pesquisa cientifica|Dissertacao (Mestrado em Educacao)|Universidade Federal do Ceara|Fortaleza|2002).

Repeated website citation $[site](https://hackage.haskell.org/package/megaparsec|Mark Karpov|Megaparsec: Monadic parser combinators|2024|27 Feb. 2026) should dedupe.

A literal dollar like $5 and $apple should stay intact.

::References::
