# PIT --------------------------------------------------------------------------

# Ex: gerar dados de uma normal padrão pelo PIT.

# Defina o número de amostras desejado
n_amostras <- 1000

# Passo 1: Gerar amostras de uma distribuição uniforme entre 0 e 1
amostras_uniformes <- runif(n_amostras)

# Passo 2: Aplicar a função de distribuição acumulada inversa da distribuição normal
amostras_normais <- qnorm(amostras_uniformes, mean = 0, sd = 1)

# Exibir as primeiras 10 amostras geradas
print(amostras_normais[1:10])


# 2.3 Testes de Hipotese Bondade de Ajuste -------------------------------------

# testar se uma amostra vem de uma determinada distribuicao


# 2.3.1 Teste Qui-quadrado -----------------------------------------------------
# (va's discretas)

# testa se as diferencas entre os valores esperados e observados de uma tabela 
# de contingencia sao significativas

# H0: os dados seguem uma dada distribuicao especificada
# H1: os dados nao seguem a tal distribuicao especificada

#chisq.test(valores observados)

# restricoes: 

#  quando GL = 1, cada freq esperada deve ser pelo menos 5
#  quando GL > 1, ate 20% das celulas pode possuir freq esperada menor que 5,
# nenhuma freq esperada pode ser menor que 1.


# 2.3.2 Estatisticas tipo Kolmogorov-Smirnov -----------------------------------
# (va's continuas)

# para distribuicoes especificadas (parametros definidos)

# Kolmogorov-Smirnov -----------------------------------------------------------

# H0: Fx = F0 para todo x real
# H1: Fx != F0, para algum x

#ks.test(x,pdist,parametros)

# 2.3.3. Anderson-Darling ------------------------------------------------------

# ad.test()


# 2.3.4 Testes de Bondade de Ajuste para Familias de Distribuicoes -------------

# testar se os dados vem de dist com parametros desconhecidos

# Testes para Normalidade ------------------------------------------------------

# lillie.test()
# padroniza a estatistica do ks.test e verifica se o resultado segue uma t(n-1)

# shapiro.test() + poderoso
# baseia-se nos valores esp e cov das estatisticas de ordem de uma a.a.~ N(0,1)

# dagostino.pearson.test() 
# testes para assimetria e curtose.

# Teste baseado em Anderson-Darling 

#ad.test{nortest}
# altera o valor de uma varivael da estatistica do ad.test original

# Cramer-von Mises 

#cvm.test{nortest}
# outra variacao do ad.test original.


# Teste para Exponencial -------------------------------------------------------

# testar se os dados vem de uma exponencial(1)
# aplica o ks.test para z = x/x_barra
# x eh o vetor de dados


# Teste de Simetria ------------------------------------------------------------

# symmetry.test{lawstat}
# H0: dist eh simetrica

# Testes de Aleatoriedade ------------------------------------------------------

# runs.test{pracma}
# Wald-Wolfowitz Test for Randomness
# H0: dist eh aleatoria

# bartels.test{lawstat}
# Bartels's Test for Randomness
#H0: dist eh aleatoria


# Testes Cox-Stuart ------------------------------------------------------------

# Ponto de Mudanca

# variaveis dicotomicas (0,1) e continuas

#H0: Nao ha mudanca abrupta na sequencia dos dados
#H1: c.c.


# Teste de Tendencia (crescente oou decrescente)

# cox.stuart.test{randtests}.
# H0: aleatoriedade
# H1: presenca de tendencia (cresc ou decresc)

























