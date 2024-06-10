
# Testes para Duas Amostras Pareadas -------------------------------------------

# (X1,Y1), (X2,Y2), ..., (Xn, Yn)

# ESPECIFICAR NA PROVA SE A AMOSTRA EH PAREADA OU INDEPENDENTE

# TESTE DO SINAL ---------------------------------------------------------------

# va's discretas ou continuas
#(desde que vc atribua o que eh + e o que eh -)

# eh um teste fraco (ultima opcao a ser considerada)

# atribuir a cada par (Di = Xi - Yi) o sinal da diferenca
# Di's = 0 sao descartados (e subtraidos do tamanho da amostra)

# T = total de +'s (estatistica do teste)
# H0: T ~ Binomial (n,0.5) 
# teste exato binomial - nao viesado e consistente (1a possibilidade)


# H0: E(Xi) = E(Yi) 
# teste de locacao
# nao eh nao viciado nem consistente
# valores esperados podem ser substituidos pelas medianas


# TESTE MCNEMAR ----------------------------------------------------------------

#mcnemar.test{stats}

# variacao do teste do sinal
# variaveis do tipo 'antes' e 'depois'
# se leva em conta se houve uma diferenca, sem dizer nada sobre o grau da diferenca

# para a prova: fazer a tabela caracteristica deste teste

# H0: P(opcao a -> opcao b) = P(opcao b -> opcao a)

# Seja A (opcao a -> b) e D (opcao b -> a)

# o teste se baseia no uso do teste binomial exato ou da aprox qui-quadrado

# A ~ Bin(A+D, 0.5) (teste bin exato)
# X2 = (|A-D|-1)^2/(A+D) ~ chisq com 1 GL


# TESTE DE POSTOS COM SINAIS DE WILCOXON ---------------------------------------

# antes e depois
# avalia a magnitude das diferenças das observações.
# variaveis intervalares
# distribuicao de Di simetrica (realizar teste de simetria)
# e E(Di) a mesma para todas as observacoes

# H0: E(D) = 0     (E(Xi) = E(Yi))

# caso Di iid, teste parametrico equivalente eh o t-pareado

# wilcox.test{stats}


# TESTE DE ALEATORIZACAO -------------------------------------------------------

# amostras feitas a partir da "mistura" de duas va's independentes
# teste exato e muito poderoso (pois baseado na aleatorizacao)
# 2 restricoes: variaveis intervalares e dist de T simetrica 

# T = sum(Di)
# H0: H0: E(D) = 0     (E(Xi) = E(Yi))

# distribuicao eh construida de acordo com as 2^n possiveis combinacoes
# de sinais para os Dis

# calcular os quantis alpha/2 e (1-alpha)/2

# perm.test{exactRankTests} 
#para formar o vetor com as 2^n possiveis combinacoes de Dis

# qperm{coin} 
#para calcular os quantis da estatistica de teste

