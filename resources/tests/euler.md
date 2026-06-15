Documento de Teste do Euler
By: Equipe Markers
At: Instituto de Testes
Location: Brasil
Date: 2026
Description: Documento para validar a linguagem Euler, math inline, substituicao e utensilios.
Keywords: euler, matematica, mathlist

::Summary::
::MathList::

---

# Definições e substituição
Primeiro definimos algumas grandezas:

!math(
def pi := 3.14159
def A := pi*r^2
def v := [vx; vy; vz]
)

Agora a área do círculo !math(A) aparece inline no meio do texto, e expandida em bloco:

!math(C = 2*A)

O vetor coluna definido é !math(v), reutilizado por substituição.

# Equações de display
A energia de repouso !math(E = m*c^2) e a fórmula de Bhaskara:

!math(
x = (-b +- sqrt(b^2 - 4*a*c)) / (2*a)
)

# Utensílios matemáticos
Matriz identidade e um limite:

!math(
I = [1, 0; 0, 1]
)

!math(
lim_(x -> 0) sin(x)/x = 1
)

Acentos, módulo, piso/teto e reticências:

!math(
vec(u) + hat(n) = bar(w)
)

!math(
|x| >= 0  e  floor(3.7) + ceil(2.1) = 6
)

!math(
S = a_1 + a_2 + ... + a_n
)

Implicações: !math(a = b => a^2 = b^2).

::References::
