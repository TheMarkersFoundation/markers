Título do Documento
By: Nome do Aluno
At: CENTRO ESTADUAL DE EDUCAÇÃO TECNOLÓGICA PAULA SOUZA\nFaculdade de Tecnologia Baixada Santista Rubens Lara\nCurso Superior de Tecnologia em Análise e Desenvolvimento de Sistemas
Location: Localização da Faculdade
Date: 2026
Description: Trabalho de Conclusão de Curso apresentado à Faculdade, como exigência para a obtenção do Título do Curso.
Mentor: Nome do Mentor
Keywords: palavras, chave, tcc

::Summary::
::FigureList::
::MathList::

---

# Isso é um capítulo
Os capitulos já são numerados automaticamente e todo o texto dentro deles também é identado
## Subcapítulos
Os subcapítulos também podem ser adicionados com mais Hashtags. Um novo capítulo é gerado quando você inicia outro com mais um só

# Exemplos de formatação de texto
- Negrito: *negrito*
- Itálico: _itálico_
- Sublinhado: __sublinhado__
- Código Inline: `codigo`
- Cor: {#FF6B99}(texto)

## Links e Imagens
Links podem ser feitos com a sintáxe [texto escrito](https://seulink.com), que se torna [texto escrito](https://seulink.com). Já as imagens podem ser adicionadas com:
![Logotipo da Linguagem Markers | Autor](../img/logo.png)

Para detalhes do parser, veja $[Megaparsec](https://hackage.haskell.org/package/megaparsec|Mark Karpov|Megaparsec: Monadic parser combinators|2024|27 Feb. 2026). Isso também é uma referência bibliográfica, que aparecerá na seção de referências.

# Notas de Rodapé e Citações
As notas de rodapé são escritas logo após a palavra anotada, no formato:

```
Texto comum^[Conteúdo da nota.]
```

Na prática, a numeração e o posicionamento são automáticos^[Esta é uma nota de rodapé: ela é numerada automaticamente e aparece no rodapé da página onde o marcador estiver, podendo conter formatação como *negrito* ou uma referência $[Megaparsec](https://hackage.haskell.org/package/megaparsec|Mark Karpov|Megaparsec: Monadic parser combinators|2024|27 Feb. 2026).] - basta inserir o marcador no meio do texto.

Já as citações diretas longas (mais de três linhas) seguem o recuo da ABNT. Cada linha começa com `> ` e uma linha final iniciada por `-- ` é interpretada como a fonte:

```
> Citação com recuo de 4 cm, letra menor e espaçamento simples.
> -- AUTOR, ano, p. X
```

O resultado é exibido assim:

> A citação direta com mais de três linhas deve ser destacada com recuo de 4 cm da margem
> esquerda, com letra menor que a do texto utilizado e com espaçamento simples, sem as aspas.
> -- ABNT NBR 10520, 2002, p. 9

# Tabelas
As tabelas são feitas com a seguinte sintaxe:
[Header 1 | Header 2 | Header 3]
[Corpo | Corpo 2 | Corpo 3 ]

# Fórmulas com Euler
As fórmulas são escritas na linguagem Euler. Uma equação em destaque ocupa sua própria linha, é numerada automaticamente e aparece na Lista de Equações:

!math(E = m*c^2)

Também é possível inserir matemática no meio de um parágrafo, como em !math(a^2 + b^2 = c^2), de forma fluida com o restante do texto.

## Definições e substituição
Definições nomeadas usam `:=` e podem ser referenciadas depois - no texto ou em outras equações - sendo expandidas automaticamente:

!math(
def phi := (1 + sqrt(5)) / 2
)

Assim, ao referenciar phi obtemos !math(phi), expandida a partir de sua definição.

## Frações, raízes, matrizes e mais
A fórmula de Bhaskara reúne fração, raiz e o operador de mais-ou-menos:

!math(
x = (-b +- sqrt(b^2 - 4*a*c)) / (2*a)
)

Euler também escreve matrizes e limites:

!math(
I = [1, 0; 0, 1]
)

!math(
lim_(n -> inf) (1 + 1/n)^n = e
)

::References::