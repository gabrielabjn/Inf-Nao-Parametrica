####
# Testes para Várias Amostras Pareadas------------------------------------------
#

# Testar se K >=3 amostras foram extraidas da mesma populacao

# Primeira eh realizado um teste para ver se existe diferenca entre as amostras,
#(caso exista, eh feito um teste de comparacoes multiplas)

# As vezes os tratamentos podem ser chamados de grupos

# Teste Q de Cochran -----------------------------------------------------------

# Para variaveis binarias
# Verifica a eficiencia de um tratamento entre grupos

# H0: os tratamentos sao igualmente efetivos
# H1: Existe diferenca de efetividade entre tratamentos

# H0: p1 = p2 = ... = pk
# H1: p1 != p2 != ... != pk

# Distribuicao exata dificil de ser calculada
# Para n grande, utiliza-se a aproximacao pela qui-quadrado com (k-1) GL

# Se rejeitar H0, podemos aplicar o Mcnemar para comparacoes 2 a 2 
#(e assim ver onde esta a diferenca)

# cochran.mctest{notas de aula} (tem inclusas as comparacoes via Mcnemar)
# cochran.qtest{RVAideMemoire} (comparacoes multiplas via teste do sinal de Wilcoxon)


# Teste ANOVA 2 Fatores por postos de Friedman ---------------------------------

# Objetivo: Testar se duas amostras foram extraidas da mesma populacao ou 
#populacoes com a mesma mediana

# variaveis pelo menos ordinais

# H0: os tratamentos sao igualmente efetivos
# H1: Pelo menos um tratamento tem maiores valores que outro tratamento

# H0: theta1 = theta2 = ... = thetak
# H1: thetai != thetaj para alguns i e j

# onde thetaj eh a mediana do tratamento j para dado individuo i

# Distribuicao exata da estatistica eh dificil de ser calculada
# Para n suf grande, segue uma qui-quadrado com (k-1) GL (aproximacao pobre)
# ou uma F(k-1, (n-1)(k-1)).

# friedman.test{agricolae}


# Comparacoes Multiplas para teste de Friedman ---------------------------------

# onde estao as diferencas?
# comparacoes 2 a 2 sempre bilaterais

# H0: thetai = thetaj, i!=j, com i, j \in {1,...,k}
# H1: thetai != thetaj

# friedman{agricolae}

# Comparacoes Multiplas com um grupo controle (para teste de Friedman) ---------

# Testar novos procedimentos contra um padrao ou controle
# k-1 testes de comparacoes multiplas

# Suponha que o tratamento 1 seja o padrao, entao teriamos
# H0: theta1 = thetaj, j \in {2,...,k}
# H1: theta1 != thetaj

# Rejeitar H0 se |R1 - Rj| >= q(alpha, c)*sqrt(nk(k+1)/6)  (onde Rj eh a soma dos postos de cada tratamento)
# onde c = k-1 eh o numero de comparacoes
# q(alpha, c) pode ser obtido na tabela AIII de Siegel


# Teste de Quade ---------------------------------------------------------------

# Alternativa ao teste de Friedman
# Objetivo: Testar se duas amostras foram extraidas da mesma populacao ou 
#populacoes com a mesma mediana

# variaveis pelo menos intervalares

# H0: os tratamentos sao igualmente efetivos
# H1: Pelo menos um tratamento tem maiores valores que outro tratamento

# H0: theta1 = theta2 = ... = thetak
# H1: thetai != thetaj para alguns i e j

# onde thetaj eh a mediana do tratamento j para dado individuo i

# Distribuicao exata dificil de ser calculada
# Para n suf grande, segue assintoticamente uma F(k-1,(n-1)(k-1))  (n eh numero de grupos)

# quade.test{stats}


# Comparacoes Multiplas para teste de Quade ------------------------------------

# rejeitou H0. Onde estao as diferencas?

# H0: thetai = thetaj, i!=j, com i, j \in {1,...,k}
# H1: thetai != thetaj

# quade.mc{notas de aula} (para n suf, segue uma t com (n-1)(k-1) GL)


# Teste de Page ----------------------------------------------------------------

# Tratamentos sao categorias de uma variavel ordinal
# Variavel de interesse eh pelo menos ordinal

# Espera-se que a media (ou mediana) da variavel de interesse cresca 
#(ou decresca) com o aumento da categoria tratamento

# H0: theta1 = ... = thetak
# H1: theta1 <= ... <= thetak

# Page.test{notas de aula}

# pequenas amostras: Siegel e Castellan
# grandes amostras: aproximacao pela N(0,1)



# Testes para Amostras Independentes -------------------------------------------

# k >= 3 amostras
# testar se as populacoes tem a mesma distribuicao
# caso existam diferencas, fazer comparacao multipla


# Teste Mediana ----------------------------------------------------------------


# Variaveis pelo menos ordinais 

# H0: todas as k populacoes tem medianas iguais
# H1: pelo menos uma populacao tem mediana diferente das demais

# qui-quadrado(k-1)
# median.test{notas de aula}


# ANOVA1 ou Kruskal-Wallis por Postos ------------------------------------------

# Variaveis pelo menos ordinais
# Testar Medias

# H0: Todas as funcoes distribuicao das K populacoes sao identicas
# H1: Pelo menos uma populacao tende a ter observacoes maiores

# pequenas amostras (k = 3 de tam <=5): Tabela A8 Conover
# grandes amostras, use a aproximacao pela qui-quadrado(K-1) (K maiusc é numero de populacoes)

# kruskal.test{stats}

# CASO H0 SEJA REJEITADA NO KRUSKAL-WALLIS -------------------------------------

# H0: thetai - thetaj para todas as combinacoes de i e j
# H1: thetai != thetaj  para algum i e algum j

# estatistica T(v) onde v = N-K (K numero de populacoes)

# kruskal{agricolae} ou kruskal.mc{pgirmess}


# Teste de Jonckheere-Terpstra -------------------------------------------------

# Variaveis pelo menos ordinais
# testar crescimento (decrescimento) de medias
# espera-se que a media/mediana cresca (ou decresca) com o aumento da categoria
#tratamento

# H0: F1(x) = F2(x) = ... = F(K)(x) para todo x
# H1: F1(x) >= F2(x) >= ... >= FK(x) para todo x

# jonckheere.test{clinfun}


# Teste de Fligner-Killeen -----------------------------------------------------

# homogeneidade de variancias
# o mais robusto

# H0: Todas as funcoes distribuicao das K populacoes sao identicas em distribuicao,
#exceto possivelmente nas medias
# H1: Pelo menos uma variancia eh diferente das demais

# qui-quadrado com K-1 graus de liberdade

# fligner.test{stats}


# Teste de Postos Quadrados de Conover -----------------------------------------

# variaveis pelo menos intervalares
# hyomogeneidade de variancias

# qui-quadrado com K-1 GL



# Tabelas de Contingência 2x2 --------------------------------------------------

# Populacoes sendo classificadas em categorias com probabilidade p

# teste bilateral
# H0: p1 = p2
# H1: p1 != p2

# teste unilateral
# H0: p1 >= p2
# H1: p1 < p2

# Teste Exato de Fisher --------------------------------------------------------

# mesmas hipotese definidas anteriormente
# segue assintoticamente um normal(0,1)
# fisher.test{stats}

# Teste Mantel-Haenzel ---------------------------------------------------------

# teste de fisher replicado (k >= 2 vezes)
# para cada experimento, gera uma tabela

# H0: p1i = p2i, para todo i = 1,...,k
# H1: p1i>p2i, para algum i

# segue assintoticamente uma normal(0,1)
# cmh.test{lawstat} ou mantelhaen.test{stats}


# Tabelas de Contingencia r x c ------------------------------------------------

# variaveis categoricas (nominais ou ordinais)


# Teste Qui-Quadrado -----------------------------------------------------------

# Diferencas em probabilidades -------------------------------------------------

# H0: p1j = p2j = ... = prj, para todo j = 1,...,c
# H1: plj != pmj para algum j e para algum par l e m

# estatistica de teste: qui-quadrado com k = (r-1)(c-1) GL

# nao usar se algum valor espérado for menor que 1 ou se mais que 20% dos valores
#esperados forem menores que 5;

# chisq.test{stats}


# Independencia ----------------------------------------------------------------

# cada observacao eh classificada em uma categoria de X e uma categoria de Y

# H0: pij = pipj , para todo i e todo j
# H1: pij != pipj para algum i e j

# estatistica de teste: qui-quadrado com k GL, onde k = (r-1)(c-1)

# nao usar se algum valor espérado for menor que 1 ou se mais que 20% dos valores
#esperados forem menores que 5;

# chisq.test{stats}