
# Questao 01 -------------------------------------------------------------------

# Exemplo de dados
cervejas <- c("L", "A", "P", "M")
individuos <- c("Ind1", "Ind2", "Ind3", "Ind4")

# Criar um data frame vazio
dados <- matrix(ncol=4, nrow=4)

# Adicionar colunas para cada tratamento
for (i in 1:4) {
  
  for (j in 1:4) dados[i,j] <- sample(c(0,1),1, replace=TRUE)
}

rownames(dados)<-individuos
colnames(dados)<-cervejas

print(dados)

# Se trata de um contexto em que tenho K "tratamentos" (cervejas) e para cada 
#individuo independente, a resposta a cada tratamento é uma variável binária.
# Logo, podemos aplicar o teste Q de Cochran.

# As hipóteses de interesse são definidas a seguir. Queremos testá-las
#a 5% de significância.

# H0: nao existe diferenca de preferencia por cervejas (p1 = p2 = p3 = p4 para cada individuo)
# H1: existe diferenca de preferencia por cervejas (pi != pj para algum individuo)

cochran.mctest <-function(X,alpha=0.05){ # alpha eh o nivel de significancia
  # Entre com a matriz de 0 e 1s
  n=nrow(X)
  G=colSums(X)
  L=rowSums(X)
  G2=G^2
  L2=L^2
  k=length(G)
  N=length(L)
  Q=((k-1)*(k*sum(G2)-sum(G)^2))/(k*sum(L)-sum(L^2))
  p_valor=1-pchisq(Q,k-1)
  print(c("Estatistica Q","p_valor"))
  print(c(Q,p_valor))
  
  if ((p_valor <= alpha)&&(k>2)){
    comb <- combn(k,2)
    nn <- ncol(comb)
    dif <- rep(0, nn)
    for (k1 in 1:nn) {
      i <- comb[1, k1]
      j <- comb[2, k1]
      Xij=X[,c(i,j)]
      tabij=matrix(0,2,2)# tabela McNemar
      s1=apply(Xij,1,sum)
      tabij[1,1]=sum(s1==0)
      tabij[2,2]=sum(s1==2)
      s2=Xij[,1]-Xij[,2]
      tabij[2,1]=sum(s2==1)
      tabij[1,2]=sum(s2==-1)
      aux2=mcnemar.test(tabij)  # esta funcao trabalha com os pares concordantes
      dif[k1] <- aux2$p.value<alpha/nn # Correcao Bonferroni
    }
    M=matrix(0,nn,3,dimnames = list(rep("",nn),c("Treat A", "Treat B", "Result")))
    cat("\nResult = 0: Non-difference;Result = 1: Significative difference\n\n")
    M[,1]=comb[1,]
    M[,2]=comb[2,]
    M[,3]=dif
    print("Multiple Comparisons")
    print(M)
  } 
}
cochran.mctest(t(dados))

qchisq(0.95,df=4-1)
# como a estatistica retornada pelo teste eh menor que o valor critico
#da qui-quadrado com 3 GL a 5%, nao rejeitamos a hipotese de que nao existe
#diferenca entre os tipos de cerveja.


# Questao 02 -------------------------------------------------------------------

data(iris)

# Separar os valores de petal_width para cada espécie
setosa <- iris$Petal.Width[iris$Species == "setosa"]
versicolor <- iris$Petal.Width[iris$Species == "versicolor"]
virginica<- iris$Petal.Width[iris$Species == "virginica"]

# se trata de um caso de amostras independentes. vamos testar, a 7%, as hipoteses
#abaixo.

# H0: as variancias populacionais dos 3 grupos sao iguais
# H1: pelo menos uma variancia populacional eh diferente das demais

# o teste de fligner-killeen eh o mais poderoso dentre os testes para homogeneidade de variancia (amostras ind)

library(stats)

fligner.test(list(setosa, versicolor, virginica))

qchisq(p=0.07,df=3-1)

# como a estatistica devolvida pelo testar esta acima do valor critico para o
#nivel de significancia adotado (7%), rejeitamos a hipotese de que as variancias 
#sao as mesmas (pelo menos uma eh diferente).


# Questao 03 -------------------------------------------------------------------

paciente <- c(seq(1:12))
drogas<-c(seq(1:3))

# Criar um data frame vazio
dados <- matrix(ncol=12, nrow=3)

dados[1,]<-c(170, 19, 187, 10, 216, 49, 7, 474, 0.4,1.4, 27, 29)
dados[2,]<-c(7, 1.4, 205, 0.3,0.2, 33, 37, 9, 0.6,63, 145, 0)
dados[3,]<-c(0, 6 ,18 ,1 ,22 ,30 ,3 ,5 ,0 ,36 ,26 ,0)

rownames(dados)<-as.character(drogas)
colnames(dados)<-as.character(paciente)

print(dados)

# as variaveis sao ordinais e as respostas as drogas, para um mesmo paciente,
#sao dependentes

# podemos aplicar a analise de variancia por dois postos de friedman

# seja theta_i a mediana da resposta a droga da i-esima linha.  Vamos testar
#a 5% as hipoteses abaixo

# H0: thetaA = thetaB = thetaC para cada paciente
# H1: thetaZ != thetaW para certas drogas Z e W

library(agricolae)
friedman.test(t(dados))
qchisq(0.95,df=3-1)
# estatistica do teste esta acima do valor critico a 1%-> rejeita a hipotese
#de que nao existe diferenca significativas entre as drogas.

ind<-c(rep(1:12,3))
trat<-c(rep("A", 12),rep("B",12),rep("C",12))
Y<-c(dados[1,],dados[2,],dados[3,])
res<-friedman(ind,trat,Y)
res

boxplot(dados[1,], dados[2,], dados[3,], 
        names = c("A", "B", "C"),
        col = c("lightpink", "lightpink", "lightpink"),
        main = "Respostas a Drogas"
        )


# tratamento C eh diferente dos demais.


# Questao 04 -------------------------------------------------------------------

# Teste de Kruskal-Wallis por Postos

# amostras independentes e variaveis pelo menos ordinais

data(iris)

# Separar os valores de petal_width para cada espécie
setosa <- iris$Petal.Width[iris$Species == "setosa"]
versicolor <- iris$Petal.Width[iris$Species == "versicolor"]
virginica<- iris$Petal.Width[iris$Species == "virginica"]

# Testar a 5%:

# H0: as K populacoes tem a mesma media
# H1: as K populacoes nao tem todas a mesma media

library(stats)
kruskal.test(list(setosa,versicolor,virginica))
# rejeita H0

res<-kruskal(iris$Petal.Width, iris$Species)
res
# os tres grupos sao diferentes entre si

# Questao 05 -------------------------------------------------------------------

# teste Jonckheere-Terpstra

# mes eh uma variavel ordinal
# variavel de interesse (pulsacoes por individuo/mes) tbm é ordinal

# amostras (pulsacoes por individuo/mes) sao independentes

# teste mencionado acima eh aplicavel. Vamos testar a 5% de significancia:

# H0:  Não há uma tendência ordenada de diminuição na taxa de pulsação ao longo dos períodos. (considerando que sao 2 meses apenas)
# H1: Existe uma tendência ordenada de diminuicao na taxa de pulsação ao longo dos períodos.

# Dados fictícios de taxa de pulsação (batimentos por minuto)
inicio <- c(75, 80, 72, 78, 85, 77, 79, 81)  # taxa de pulsação no início
fim <- c(70, 75, 68, 76, 80, 72, 74, 78)      # taxa de pulsação no final

# Visualização dos dados
dados <- data.frame(Inicio = inicio, Fim = fim)
dados

library(clinfun)
resultado_teste <- jonckheere.test(dados$Inicio,dados$Fim)
resultado_teste

# P-VALOR ABAIXO DE 5% -> REJEITAMOS H0 E CONCLUIMOS QUE FAZER EXERCICIOS
# REGULARMENTE DIMINUI A TAXA DE PULSACAO DE UM INDIVIDUO.



