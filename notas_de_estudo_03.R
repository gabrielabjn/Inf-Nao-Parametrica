
# tipos de variaveis -----------------------------------------------------------

# categoricas - nominal e ordinal 
# numericas - discreta
#             continua
#             intervalar (valores representam categorias em uma escala continua)
#             razao (0 absoluto)

# clecio usa escala de 'intervalo' como sinonimo de 'razao'
# ------------------------------------------------------------------------------


# Testes para duas amostras independentes --------------------------------------

# Testes Aderencia -------------------------------------------------------------

# objetivo: testar se duas amostras ind foram extraidas da mesma populacao
# (com mesma distribuicao)


# KS TEST ----------------------------------------------------------------------

# variaveis no minimo ordinais 
# restricao: sensivel a assimetria, dipersao, curtose, etc

# ks.test(x,y)

# Tabelas A19 e A20 (p.568 pdf) em Conover


# Cramer-Von Mises -------------------------------------------------------------

# variaveis no minimo ordinais
# restricao: somente hipoteses bilaterais

# Cramer.estat{Notas de aula}  (p obter estatistica de teste)
# ou
# pcramer{coda}  (fda)
# ou
# cramer.test{cramer}


# Testes Locação ---------------------------------------------------------------


# objetivo: testar se duas distribuicoes diferem nas tendencias
#centrais (media/mediana)


# Teste da Mediana -------------------------------------------------------------

# variaveis no minimo ordinais
# restricao: teste nao eh exato (mas devolve estatistica exata)

# median.test2sample{Notas de aula}
# ou 
# median_test{coin}


# Teste Wilcoxon-Mann-Whitney --------------------------------------------------

# variaveis no minimo ordinais (postos)
# testar se Fx(x) = Fy(y) POREM tambem pode ser aplicado para testar 
#medias populacionais (H0: EX(x) = EY (y), que impoe var iguais)
# restricao: diferencas entre as distribuicoes somente nas locacoes 

# wilcox.test{stats}
# Tabela A7 em Conover, para n,m ≤ 20 (p. 548 pdf)
# w1-p = n(n+1)/2 - wp 

# OBS : # quando você chama wilcox.test(x, y), o R considera os postos da variável x para calcular a estatística do teste
# se você inverter as amostras e chamar wilcox.test(y, x), o resultado será o mesmo, mas agora os postos de y serão usados para calcular a estatística do teste.

# Teste van der Waerden --------------------------------------------------------

# teste locacao

# Valores críticos em Waerden and Nievergelt (1956).
# p grandes amostras -> aprox. pela normal

# waerden.test{agricolae}
# ou
# normal−test{coin}


# Teste Posto-Ordem-Robusto (POR) ----------------------------------------------

# variaveis pelo menos ordinais
# testa medianas populacionais
# testa amostras e nao postos
# restricao: distribuicoes simetricas, porem podem diferir em
# outros momentos alem das locacoes (ex: var, assimetria, curtose)

# fp.test{RVAideMemoire}

# Tabela K em Siegel and Castellan (m ≤ n ≤ 12)


# Teste de aleatorizacao -------------------------------------------------------

# variaveis pelo menos intervalares
# testa medias populacionais
# trabalha com os valores amostrais de uma dist ao inves dos postos (WMW)

# oneway−test{coin} 
# perm.test{exactRankTests}.



# Testes para Variância  -------------------------------------------------------


# ou diferença de escala

# H0: sigma_x^2 = sigma_y^2
# H1 eh bilateral ou unilateral (esquerdo/direito)


# Teste de Mood ----------------------------------------------------------------


# variaveis pelo menos ordinais
# estatistica baseada em POSTOS
# pequenas amostras - tabela em Laubscheret al.
# grandes amostras - aproximacao pela normal

# mood.test{stats} ou mood−test{coin}


# Teste de escores normais de Klotz --------------------------------------------


# variaveis pelo menos ordinais
# mais forte dentre os testes ordinais p variancia
# estatistica baseada em POSTOS
# quadrado dos pesos
# pequenas amostras (N = n+m <= 20) - Klotz (1962)
# grandes amostras - aproximacao pela normal

# klotz−test{coin}


# Teste de Siegel-Tukey --------------------------------------------------------


# variaveis pelo menos ordinais
# medianas iguais ou conhecidas
# a estatistica do teste eh a mesma do wilcox_test
# Tabela A7 em Conover, para n,m ≤ 20 (p. 548 pdf)

# siegel.test{jmuOutlier}


# Teste de Ansari-Bradley ------------------------------------------------------


# variaveis pelo menos ordinais
# o teste assume que as medianas sao iguais ou conhecidas
# alternativa ao Siegel-Tukey

# pequenas amostras (N = n + m <= 20) - Ansari e Bradley (1960)
# grandes amostras - aproximacao pela normal

# cAnsBrad{NSM3} (use a funcao cAnsBrad(α, n,m) para obter os valores criticos)
# ansari−test{coin}

# se as medianas forem desconhecidas, substrair as medianas amostrais de cada
#grupo de suas observacoes (mas o teste deixaria de ser livre de distribuicao
#em pequenas amostras)
# ser livre de distribuicao significa que o teste é robusto o suficiente para 
#fornecer resultados válidos e confiáveis, mesmo quando os dados não seguem uma
#distribuição particular,


# Teste de Posto-Similaridade de Moses -----------------------------------------


# mais robusto que o de Siegel-Tukey porem mais restritivo
# variaveis pelo menos intervalares

# Moses.test(x, y, k) (notas de aula) # k eh o nº de subgrupos definido (deve ser >=2)
# MosesTest{DescTools}


# Testes de Postos Quadrados ---------------------------------------------------


# variaveis pelo menos intervalares
# pequenas amostras (n,m ≤ 10) - Tabela A9 em Conover (1999)
# grandes amostras - aproximacao pela normal

# SquaredRanks.estat(x,y) {notas de aula}
# conover−test{coin}