
2.2 Estatisticas de Ordem e Funcao Distribuicao

a. Graficos fdp, fda e quantil teoricos
# Exponencial
lambda=0.5
x=seq(0,17,length=200)
plot(x,dexp(x,lambda),type='l',lwd=3,col='blue',xlab='x',ylab='f_X')

plot(x,pexp(x,lambda),type='l',lwd=3,col='blue',xlab='x',ylab='F_X')
abline(h=1,lty=3)

p=seq(0.01,0.999,length=100)
plot(p,qexp(p,lambda),type='l',lwd=3,col='blue',xlab='p',ylab='Q_X')
abline(v=1,lty=3)

# Normal padrao
x=seq(-4,4,length=200)
plot(x,dnorm(x),type='l',lwd=3,col='blue',xlab='x',ylab='f_X')

plot(x,pnorm(x),type='l',lwd=3,col='blue',xlab='x',ylab='F_X')
abline(h=1,lty=3)

p=seq(0.01,0.999,length=100)
plot(p,qnorm(p),type='l',lwd=3,col='blue',xlab='p',ylab='Q_X')
abline(v=1,lty=3)





b. Aplicacao - Teorema da Inversao
rm(list=ls(all=TRUE) )
#### n = 100
n=100
U=runif(n,0,1)  
lambda=5
Xi=-1/lambda*log(1-U)
X=rexp(n,lambda)
par(mfrow=c(2,1))
hist(Xi, main="Gerando X pelo Teorema da Inversão")
hist(X, main="Gerando X direto pelo gerador da Distribuição")
Yi=ecdf(Xi)
Y=ecdf(X)
plot(Yi)
plot(Y)

plot(Yi,col="red",lwd=3)
lines(Y,col="yellow",lty=2,lwd=3)

###### n = 1000
rm(list=ls(all=TRUE))
n=1000
n=100
U=runif(n,0,1)
lambda=5
Xi=-lambda*log(1-U)
X=rexp(n,lambda)
par(mfrow=c(2,1))
hist(Xi, main="Gerando X pelo Teorema da Inversão")
hist(X, main="Gerando X direto pelo gerador da Distribuição")

Yi=ecdf(Xi)
Y=ecdf(X)
plot(Yi)
plot(Y)

plot(Yi,col="red",lwd=3)
lines(Y,col="yellow",lty=2,lwd=3)

# Notem que a função distribuição acumulada mostra uma melhor aproximação entre as duas amostras


c. Convergência da fde - Glivenko-Cantelli 
# Função distribuição empírica
# Normal
n=1000  # 20  # 100, 1000
x=rnorm(n)
Fn=ecdf(x)
plot(Fn,lwd=2,col='blue',ylab='',main='')
#legend(-2,0.9,c(expression(F[n](x)),'fda teórica'),text.col=c('blue','black'))
xsort=sort(x)   # ordenando os dados
lines(xsort,pnorm(xsort),lwd=2)



# Exponencial
n=1000  # 20  # 100, 1000
x=rexp(n,5)
Fn=ecdf(x)
plot(Fn,lwd=2,col='blue',ylab='',main='')
#legend(-0.1,0.9,c(expression(F[n](x)),'fda teórica'),text.col=c('blue','black'))
xsort=sort(x)   # ordenando os dados
lines(xsort,pexp(xsort,5),lwd=2)


d. Funcao Quantil Empirica

# Função Quantil Empírica
n=20  # 100, 1000
x=rnorm(n)
n=length(x)
u=seq(0,1,by=1/n)         
Qn=quantile(x,u)
plot(u,qnorm(u),type="l")
points(u,Qn,pch=19)
for (i in 1:n){  
lines(c(u[i],u[i+1]),c(Qn[i],Qn[i]))
}

# Fazer para outras distribuições contínuas:
# Exponencial, Gamma, Beta, etc.


##################################################################################


2.3 Testes de hipoteses de bondade de ajuste

2.3.1 Teste Qui-quadrado de Aderência

# Estatística: X^2=sum_{i=1}^{N}(Obser_i-Esper_i)^2/Esper_i=sum_{i=1}^{N}Observ_i^2/Esper_i - N

# X^2 (sob H0) ~ X^2_{k-1 g.l.}

# Frequencias esperadas devem ser >= 5

# Teste não deve ser usado se 20% das freq. esperadas são < 5 ou
# se qualquer frequencia esperada é < 1

# Tabela C do Apendice

# Ex. 4.2a Siegel: Nº de vitórias de corridas de cavalo, por posição de largada
tab=matrix(0,nrow=2,ncol=8)
tab[1,]=c(29,19,18,25,17,10,15,11)
tab[2,]=rep(18,8)
tab

chisq.test(tab[1,])
sum((tab[1,]-tab[2,])^2/tab[2,])


## Variáveis discretas, porém com infinitos valores: agregar valores

Exemplos 2.10 e 2.11 da Apostila

######################## exemplo 2.10
x=c(181,9,4,10,7,4,5)
x.ind=0:6
lambda.est=sum(x*x.ind)/sum(x) # numero total de acidentes sobre numero de pessoas

p0=dpois(0:5,lambda.est)
p10=1-ppois(5,lambda.est)
p=c(p0,p10)
p
Esp=sum(x)*p

# Agrupar 3 ou mais, pois Esp<1 para 4 ou mais

x1=c(x[1:3],sum(x[4:7]))
Esp1=c(Esp[1:3],sum(Esp[4:7]))

X2=sum((x1-Esp1)^2/Esp1)
X2    # diferencas de arredondamento
gl=4-1-1
pv=1-pchisq(X2,gl)
pv

########################## exemplo 2.11
x=c(10,24,10,4,1,1)
x.ind=0:5

# Poisson
lambda.est=sum(x*x.ind)/sum(x)    # media amostral, numero medio de defeitos em uma amostra de tamanho 13

p0=dpois(0:4,lambda.est)
p10=1-ppois(4,lambda.est)
p=c(p0,p10)
p
Esp=sum(x)*p
Esp
# Agrupar 4 ou mais, pois Esp(5 ou mais)<1
x1=c(x[1:4],sum(x[5:6]))
Esp1=c(Esp[1:4],sum(Esp[5:6]))

X2=sum((x1-Esp1)^2/Esp1)
X2    
gl=6-1-1-1     #(k-1 + agrupamento + 1 gl pela estimacao de lambda)
pv=1-pchisq(X2,gl)
pv


# Binomial : Numero de defeitos em cada lote de n= 13 unidades segue uma distrib. Binomial(n,p)
defeitos=24+2*10+3*4+4*1+5*1
p.est=defeitos/650  # media amostral , proprocao de defeitos nos 50*13=650 testes

p0=dbinom(0:4,13,p.est)
p10=1-pbinom(4,13,p.est)
p=c(p0,p10)
p
Esp=50*p  # 50 amostras da binomial(13,p)
Esp
# Agrupar 4 ou mais, pois Esp(5 ou mais)<1
x1=c(x[1:4],sum(x[5:6]))
Esp1=c(Esp[1:4],sum(Esp[5:6]))

X2=sum((x1-Esp1)^2/Esp1)
X2    # diferencas de arredondamento
gl=6-1-1 -1  #(k-1 + agrupamento + 1 gl pela estimacao de p)
pv=1-pchisq(X2,gl)
pv

# Observacao Gibbons:
This example illustrates a common result with chi-square good-
ness-of-fit tests, i.e., that each of two (or more) different null hy-
potheses may be accepted for the same data set. Obviously, the true
distribution cannot be both binomial and Poisson at the same time.
Thus, the appropriate conclusion on the basis of a chi-square
goodness-of-fit test is that we do not have enough information to dis-
tinguish between these two distributions


#############################################################################


2.3.2 Testes KS (Kolmogorov-Smirnov) - completamente especificadas 
2.3.3 Testes AD (Anderson-Darling) - completamente especificadas
require(goftest)  # cuidado, pois existe a mesma funcao para bondade de ajuste no pacote nortest
ad1.test<-ad.test   # quando chamar nortest, posso usar ad.test (R mantem a funcao do ultimo pacote carregado)

# Exemplo 1 Conover, pag. 433

# H0: Os dados foram gerados de uma U(0,1)
# H1: Os dados não foram gerados de uma U(0,1)
x=c(0.621,0.503,0.203,0.477,0.710,0.581,0.480,0.554,0.382)
plot(ecdf(x))
x0=seq(0,1,by=0.1)
fda_teorica= x0
lines(x0,fda_teorica)

ks.test(x,"punif",0,1)
ad1.test(x,"punif",0,1)


# Exemplo (experimento dos fios): n=20

x=cbind(0.6, 0.8, 1.1, 1.2, 1.4, 1.7, 1.8, 1.9, 2.2, 2.4, 2.5, 2.9, 3.1, 3.4, 3.4, 3.9, 4.4, 4.9, 5.2, 5.9)

plot(ecdf(x))
x0=seq(0,6,by=0.1)
fda_teorica= x0/6
lines(x0,fda_teorica)

#No R, ks.test  e ad.test
ks.test(x,"punif",0,6)
ad1.test(x,"punif",0,6)

# Tabela F do Apendice (Siegel)

# Exemplo:
n=20
x=rnorm(n)

ks.test(x,"pnorm",0,1)
ks.test(x,"pnorm",mean(x),sd(x))

ad1.test(x,"pnorm",0,1)
ad1.test(x,"pnorm",mean(x),sd(x))
 
# Teste bilateral
x=rnorm(30)
# Vamos testar se os dados vieram de uma N(2,1)
xs=sort(x)
hist(x,prob=T,xlim=c(min(x),5))  
xord=seq(min(x),5,length=100)
lines(xord,dnorm(xord,2,1),type="l")

# Os dados acumulam antes da teórica

ks.test(x,"pnorm",2,1,alternative="greater") # ver interpretação no output
# ele testa se a fde dos dados é maior (acumula antes) que a Fx de X

# A-D nao faz uniliateral

2.3.4 Testes de bondade de ajuste para familias de distribuicoes

# Teste de D'Agostino et al para Normalidade

https://stat.ethz.ch/pipermail/r-help/2004-August/056749.html

dagostino.pearson.test <- function(x) {
    # from Zar (1999), implemented by Doug Scofield,  scofield at bio.indiana.edu
    DNAME <- deparse(substitute(x))
    n <- length(x)
    x2 <- x * x
    x3 <- x * x2
    x4 <- x * x3
    # compute Z_g1
    k3 <- ((n*sum(x3)) - (3*sum(x)*sum(x2)) + (2*(sum(x)^3)/n)) /
((n-1)*(n-2))
    g1 <- k3 / sqrt(var(x)^3)
    sqrtb1 <- ((n - 2)*g1) / sqrt(n*(n - 1))
    A <- sqrtb1 * sqrt(((n + 1)*(n + 3)) / (6*(n - 2)))
    B <- (3*(n*n + 27*n - 70)*(n+1)*(n+3)) / ((n-2)*(n+5)*(n+7)*(n+9))
    C <- sqrt(2*(B - 1)) - 1
    D <- sqrt(C)
    E <- 1 / sqrt(log(D))
    F <- A / sqrt(2/(C - 1))
    Zg1 <- E * log(F + sqrt(F*F + 1))
    # compute Z_g2
    G <- (24*n*(n-2)*(n-3)) / (((n+1)^2)*(n+3)*(n+5))
    k4 <- (((n*n*n + n*n)*sum(x4)) - (4*(n*n + n)*sum(x3)*sum(x)) -
(3*(n*n - n)*sum(x2)^2) + (12*n*sum(x2)*sum(x)^2) - (6*sum(x)^4)) /
(n*(n-1)*(n-2)*(n-3))
    g2 <- k4 / var(x)^2
    H <- ((n-2)*(n-3)*abs(g2)) / ((n+1)*(n-1)*sqrt(G))
    J <- ((6*(n*n - 5*n + 2)) / ((n+7)*(n+9))) * sqrt((6*(n+3)*(n+5)) /
(n*(n-2)*(n-3)))
    K <- 6 + (8/J)*(2/J + sqrt(1 + 4/(J*J)))
    L <- (1 - 2/K) / (1 + H*sqrt(2/(K-4)))
    Zg2 <- (1 - 2/(9*K) - (L^(1/3))) / (sqrt(2/(9*K)))
    K2 <- Zg1*Zg1 + Zg2*Zg2
    pk2 <- pchisq(K2, 2, lower.tail=FALSE)
    RVAL <- list(statistic = c(K2 = K2), p.value = pk2, method =
"D'Agostino-Pearson normality test\n\nK2 is distributed as Chi-squared
with df=2", alternative = "distribution is not normal", data.name =
DNAME)
    class(RVAL) <- "htest"
    return(RVAL)
}



###################
Testes para Normalidade

# Exemplo 1 Conover, pag. 444
x=c(23,23,24,27,29,31,32,33,33,35,36,37,40,42,43,43,44,45,48,48,54,54,56,57,57,58,58,58,58,59,61,61,62,63,64,65,66,68,68,70,73,73,74,75,77,81,87,89,93,97)
hist(x)
require(nortest)
?lillie.test
lillie.test(x)
 
# Conferindo com K-S
z=(x-mean(x))/sd(x)
ks.test(z,"pnorm") 

# Estatística igual, p_valor diferente

lillie.test(x)
shapiro.test(x)
dagostino.pearson.test(x)
ad.test(x)
cvm.test(x)


# Comparação entre testes
require(sn)
x=rsn(10,5,1,7)  # variar lambda's
hist(x)
lillie.test(x)
shapiro.test(x)
dagostino.pearson.test(x)
ad.test(x)
cvm.test(x)

# Misturas de Normais -> Bimodalidade
x1=0.6*rnorm(10,0,1)
x2=0.4*rnorm(10,4,1)
x=c(x1,x2)
hist(x)
lillie.test(x)
shapiro.test(x)
dagostino.pearson.test(x)
ad.test(x)
cvm.test(x)

# Achatamento/curtose
x=rt(20,3)
hist(x,nclass=15)
lillie.test(x)
shapiro.test(x)
dagostino.pearson.test(x)
ad.test(x)
cvm.test(x)


# Teste de Lilliefors para a distrib. Exponencial
x=rexp(100,5)
hist(x)
z=x/mean(x)
aa=ecdf(z)
plot(ecdf(z))
x1=seq(0,7,by=0.01)
lines(x1,pexp(x1))

# Usar tabela KS
ks.test(x/mean(x),"pexp",1) 

#############################################################################

2.4. Testes de simetria


require(nortest)
require(lawstat)
require(sn)

symmetry.test(x, option = "opt")

?symmetry.test

# Normalidade
x=rnorm(100)
hist(x)

symmetry.test(x,option="MGG")
symmetry.test(x,option="CM")
symmetry.test(x,option="M")

dagostino.pearson.test(x)
shapiro.test(x)


# Assimetria
x=rsn(100,5,1,7)# lambda=1  # mu, sigma, lambda
hist(x)

symmetry.test(x,option="MGG")
symmetry.test(x,option="CM")
symmetry.test(x,option="M")

dagostino.pearson.test(x)
shapiro.test(x)

### Simetria e caudas pesadas
x=rt(100,1) 
hist(x)

symmetry.test(x,option="MGG")
symmetry.test(x,option="CM")
symmetry.test(x,option="M")

dagostino.pearson.test(x)
shapiro.test(x)


# Misturas de Normais -> Bimodalidade (se bem separadas, eh simetrica)
x1=0.5*rnorm(100,0,1)
x2=0.5*rnorm(100,10,1)
x=c(x1,x2)
hist(x)

symmetry.test(x,option="MGG")
symmetry.test(x,option="CM")
symmetry.test(x,option="M")

dagostino.pearson.test(x)
shapiro.test(x)

##########################################################3##################

2.5 Testes de Aleatoriedade

2.5.1 Teste das corridas: para variáveis binárias e contínuas
 
2.5.2 Teste de RVN de Bartels (var. contínuas)


# Para variáveis binárias: N tamanho da amostra, n1: nº de '1', n2: nº de '0', n=n1+n2
# Para variáveis contínuas: '+' se X_i > Mediana(X), '-', caso contrário
# Estatística de teste: r= nº de séries
# Teste exato (pequenas amostras): Use a função 'densRuns' para calcular a função de probabilidade.
# Castellan usa m, n e N. Gibbons usa n1, n2 e n; é a mesma coisa
# Use a Tabela G (Siegel) do Apendice: Intervalos de Confiança
# Ou faça os cálculos usando a função densRuns

# Cálculo do nº de séries para var. binárias
NumSeriesDisc <-function(y){
# y deve ser um vetor de 0's e 1's
n1=sum(y)
n2=length(y)-n1
series=1
for (i in 2:length(y)){
if(y[i]!=y[i-1]){series<-series+1}}
r=series
#print(c("n1", "n2", "nº de séries"))
return(c(n1,n2,r))
}

# Cálculo do nº de séries para var. contínuas (usando mediana)
NumSeriesCont <-function(y){
grupo=c()
for(i in 1:length(y)){
if (y[i]>median(y)) grupo[i]<-'+'  
if (y[i]<median(y)) grupo[i]<-'-' 
if (y[i]==median(y)) grupo[i]<-0
}
grupo=grupo[grupo!=0]
n1<-0
n2<-0
for(i in 1:length(grupo)){
if(grupo[i]=='+'){n1<-n1+1}
if(grupo[i]=='-'){n2<-n2+1}}
series<-1
for(i in 2:length(grupo)){
if(grupo[i]!=grupo[i-1]){series<-series+1}
}
r=series
#print(c("n1", "n2", "nº de séries"))
return(c(n1,n2,r))
}



# Teste das Corridas para grande amostras (aproximação pela normal)
RunsTesteAprox <- function(y){
fit<-NumSeriesCont(y)
n1=fit[1]
n2=fit[2]
r=fit[3]
# hc: correção de continuidade
n=n1+n2
media=2*n1*n2/n+1
varR=2*n1*n2*(2*n1*n2-n)/(n^2*(n-1))
if (r>media) hc=-1/2 else hc=1/2
rz=(r+hc-media)/sqrt(varR)
p_valor=2*pnorm(-abs(rz))
print("p_valor")
return(p_valor)
}

# Função para calcular probabilidades de séries (r)
densRuns <-function(n1,n2,r){
n=n1+n2
# r: nº de séries ou corridas
if (r/2>as.integer(r/2)) fr=(choose(n1-1,(r-1)/2)*choose(n2-1,(r-3)/2)+choose(n1-1,(r-3)/2)*choose(n2-1,(r-1)/2))/choose(n,n1)
else fr=2*choose(n1-1,r/2-1)*choose(n2-1,r/2-1)/choose(n,n1)
return(fr)
}

# Função para calcular acumuladas de séries (r) 
cdfRuns <- function(n1,n2,qe){
#qe >=2
p=0
for (r in 2:qe) p = p+densRuns(n1,n2,r)
return(p)
}

# Função para calcular quantis de séries (r)
qRuns <- function(n1,n2,p){
r=2
p1=densRuns(n1,n2,r)
if (p1>=p) qR=r else{
while (p1 < p) {
r=r+1
p1 = p1 + densRuns(n1,n2,r)
}
if (p<0.5) qR=r-1
if (p>=0.5)qR=r+1}
return(qR)
}

# variaveis binarias (por exemplo, Bernoulli's)
require(snpar) # runs.test => mesmo nome no pacote lawstat (este soh faz assintotico)
#
runs.test1<-runs.test
require(lawstat)

y=rbinom(15,1,0.4)
plot(y,type="l")
R=NumSeriesDisc(y)
R
n1=R[1]
n2=R[2]
qRuns(n1,n2,0.025)
qRuns(n1,n2,0.975)
runs.test1(y,exact=TRUE)

################## var. numericas

# Pequenas amostras
y=runif(20)
R=NumSeriesCont(y)
R
n1=R[1]
n2=R[2]
r=R[3]
qRuns(n1,n2,0.025)
qRuns(n1,n2,0.975)
runs.test1(y,exact=TRUE)


# Amostras Grandes
y=rnorm(100)
runs.test1(y)
bartels.test(y) # statistic <- (RVN - 2)/sqrt(4/n) 
RunsTesteAprox(y) # este faz correcao de continuidade

y=rpois(100,5)
runs.test1(y)
runs.test(y)
bartels.test(y)
 

y = arima.sim(n = 100, list(ar = c(0.5)))
plot(y)
runs.test1(y)
RunsTesteAprox(y)
bartels.test(y)

# Eles podem ser maiores ou menores que a mediana/media.
# Aleatoriedade quer dizer que a ordem em que as observações aparecem é que é aleatória

############## Possiveis faltas de aleatoriedade

# Serie temporal
y = arima.sim(n = 40, list(ar = c(0.5)))
plot(y)
runs.test1(y,exact=TRUE)
bartels.test(y)


# "desaleatorizando"
y=sort(rnorm(100))
runs.test1(y,exact=TRUE)
bartels.test(y)

#  Mudanca na locacao
x1=rnorm(50)
x2=rnorm(50,5,1)
y=c(x1,x2)
plot(y,type='l')
runs.test1(y)
bartels.test(y)

# Tendencia na locacao
muv=seq(0.1,2,length=100)
y=c()
for (i in 1:100) y[i]=rnorm(1,muv[i],1)
plot(y,type='l')
runs.test1(y)
bartels.test(y)

# aumentando a tendencia
muv=seq(0.1,5,length=100)
y=c()
for (i in 1:100) y[i]=rnorm(1,muv[i],1)
plot(y,type='l')
runs.test1(y)
bartels.test(y)


# Mudanca na variacao   # nao acusa
x1=rnorm(50)
x2=rnorm(50,0,2)
y=c(x1,x2)
plot(y,type='l')
runs.test1(y)
bartels.test(y)

x1=rnorm(50)
x2=rnorm(50,0,5)
y=c(x1,x2)
plot(y,type='l')
runs.test1(y)
bartels.test(y)



# Variacao crescente    # nao acusa
sigma2v=seq(0.1,2,length=100)
y=c()
for (i in 1:100) y[i]=rnorm(1,0,sqrt(sigma2v[i]))
plot(y,type='l')
runs.test1(y)
bartels.test(y)

 

#############################################################################

2.5.3 Possiveis casos de nao-aleatoriedade

i) Teste de ponto de mudança

## para variáveis binárias e contínuas

## x é binária (0 ou 1)
# Ex.
x=cbind(0,1,0,0,0,1,0,0,0,1,1,0,1,1,0,1,0,1)
x=rbinom(20,1,0.5)

plot(1:length(x),x,"l")

TestPMDisc<-function(x){
m=sum(x)
N=length(x)
n=N-m
Sj=cbind(rep(0,N-1))
for (j in 1:(N-1)) Sj[j]=sum(x[1:j])
Sj=cbind(as.numeric(Sj))
jj=cbind(as.numeric(1:(N-1)))
Dt=N/(n*m)*abs(Sj-jj*m/N)
Dcal=max(Dt)
pm=which.max(Dt)
print(c("Estat. Dmn","ponto mudança"))
return(c(Dcal,pm))
}

Dmn_calc=TestPMDisc(x)
length(x)
sum(x)

Dmn_calc

# Estatística de Teste: D_m,n (pág. 85)
# Use Tabela G L_II do Apendice Siegel (m e n < 25)
# Se m ou n > 25, use Tabela L_III do Apendice

## Exemplo 4.6 Siegel, Tabela 4.7
A1=c(1,1,1,1,0,0,1,1,1,1,0,0,1,1,1,1,1,1,1,1,1,1,1,0,1,1,0,0,1,1,1,0,0,1,1,1,1,1,0,1,1,1,1,0,0,1,1,1,1,1,1,1,0,1,1,1,0,0,1,1)
A2=c(0,1,1,0,1,1,1,1,0,0,1,0,1,1,1,1,0,1,1,1,0,0,1,1,1,1,1,1,1,0,0,0,0,1,1,1,1,1,0,1,1,1,1,1,1,0,1,1,1,0,0,0,0,1,1,1,1,0,1,1)
A3=c(0,1,1,0,1,1,1,1,0,0,1,1,1,1,1,1,1,1,1,1,1,0,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,1,1,1,1,0,0,1,1,1,0,0,1,1,1,1,0,1,1,0,1)
A4=c(0,0,1,1,1,1,0,1,0,1,1,1,1,1,1,1,1,0,0,1,1,1,1,1,1,1,0,0,0,1,1,1,1,1,1,1,1,1,1,1,0,1,1,1,1,0,0,1,1,1,1,1,1,1,1,1,0,0,1,1)
A=c(A1,A2,A3,A4)

B1=c(0,0,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,1,1,1,0)
B2=c(1,1,0,1,1,0,0,1,1,1,1,0,0,0,0,1,1,1,1,1,0,1,1,1,0,0,1,0,1,1,1,1,1,0,0,1,1,1,1,0,1,1,0,1,1,1,0,0,1,1,1,1,0,0,0,0,0,1,0,1)
B3=c(1,1,0,1,1,0,1,1,1,1,0,0,0,0,0,0,1,1,1,1,1,0,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,0,0,1,1,0,0,1,1,1,1,0,0,1,1,1)
B4=c(1,0,0,0,0,1,1,1,1,0,1,1,0,1,1,0,0,0,0,1,1,1,0,0,0,1,1,1,1,1,1,1,0,0,0,0,1,1,1,1,0,1,1,0,1,0,0,1,0,0,0,0,0,1,1,1,0,0,1,1)
B=c(B1,B2,B3,B4)
plot(A,type="l")
plot(B,type="l")


Dmn_calc=TestPMDisc(A)
Dmn_calc
# valor critico, teste a 5%
m=sum(A)
n=length(A)-m
valor_critico=1.36*sqrt((m+n)/(m*n))
valor_critico

Dmn_calc=TestPMDisc(B)
Dmn_calc
# valor critico, teste a 5%
m=sum(B)
n=length(B)-m
valor_critico=1.36*sqrt((m+n)/(m*n))
valor_critico


## Contínuas

x=rnorm(20)
plot(x,type="l")

TestePMCont <-function(x){
N=length(x)
posto=rank(x)
Wj=cbind(rep(0,N))
for (j in 1:N) Wj[j]=sum(posto[1:j])
cbind(posto,Wj)
Wj1=Wj[1:N-1]
jj=cbind(as.numeric(1:(N-1)))
Kt=abs(2*Wj1-jj*(N+1))
Kcal=max(Kt)
m=which.max(Kt)
Kmn=Kt[m]
n=N-m
print(c("m","n","Kmn"))
return(c(m,n,Kmn))
}

x=rnorm(20)
aa=TestePMCont(x)
m=aa[1]
sum(rank(x[1:m]))

# Teste exato: Use Tabela J (Siegel)
# Se Kmn > W tabelado, rejeite H0 (há mudança na distribuição)
# Para usar a tabela J: calcule a soma dos postos até a posição m e compare com a tabela


#### Amostra grande
TestePMContaprox <-function(x){
pmx=TestePMCont(x)
m=pmx[1]
N=length(x)
n=N-m
W=rank(x)
Wm=sum(W[1:m])
EW=m*(N+1)/2
VarW=m*n*(N+1)/12
Wz=(Wm-EW-1/2*sign(Wm-EW))/sqrt(VarW)
p_valor=2*pnorm(-abs(Wz))
print(c("pto mudança","Wz","p_valor"))
return(c(m,Wz,p_valor))
}
# Exemplos
y=rnorm(100)
TestePMContaprox(y)

y = arima.sim(n = 100, list(ar = c(0.5, -0.5), ma = c(-1, 5)), sd = sqrt(0.1796))
plot(y,type="l")
TestePMContaprox(y)

# Exemplo: Dolar x Euro, de hora em hora
# Detectar quando existir uma mudança bruta
# colunas representam, respectivamente: DATE (data), TIME (hora), OPEN (abertura), HIGH (alta), LOW (baixa), CLOSE (fechamento) e VOLUME (número de pessoas que compraram/venderam as referidas moedas).
y=read.table(file="EURUSD60.csv",sep=",")
y=data.frame(y)
names(y)= c("data","hora","abertura","alta","baixa","fechamento","volume")
attach(y)
plot(fechamento,type="l")
TestePMContaprox(fechamento)

x=seq(0,2*pi,length=50)
y=cos(x)+rnorm(50)
plot(x,y)
lines(x,cos(x))
TestePMContaprox(y)

y1=rnorm(30)
y2=rnorm(30,2,1)
y=c(y1,y2)
plot(y)
TestePMContaprox(y)


ii) Teste de Tendência de Cox & Stuart
require(randtests)

parX <-function(X){
n=length(X)
cn=trunc((n+1)/2)
if (n/2==as.integer(n/2)){
parX=rep(0,cn)
for (i in 1:cn) parX[i]=X[i+cn]-X[i]}
else
{
cn1=cn-1
parX=rep(0,cn1)
for (i in 1:cn1) parX[i]=X[i+cn]-X[i]}
return(parX)
}


# Ex. 2 - Conover, pág. 171
X=c(45.25,45.83,41.77,36.26,45.37,52.25,35.37,57.16,35.37,58.32,41.05,33.72,45.73,37.90,41.72,36.07,49.83,36.24,39.90)

D0=parX(X)
plot(D0,type="l")
T0=length(D0[D0>0])
T0

# Hipótese bilateral, alpha=0.05
n=length(D0)
binom.test(T0,n,1/2)

cox.stuart.test(X)

# Ex. 3 - Conover, pág. 171/172
X=c(14.6,12.2,104,220,110,86,92.8,74.4,75.4,51.7,29.3,16,14.2,10.5,123,190,138,98.1,88.1,80,75.6,48.8,27.1,15.7)

D0=parX(X)
plot(D0,type="l")
T0=length(D0[D0>0])
T0

# H1: a taxa de descarga está decrescendo
# Hipótese unilateral esquerdo, alpha=0.01
n=length(D0)
binom.test(T0,n,1/2,alter="l")

cox.stuart.test(X,alternative='left.sided')


# Ex. 5 Conover, pág. 173/174
X=c(84,60,63,109,111,83,146,166,119,143,151,116,137,163,139,208,174,283,176,296,176,286,223,235)
plot(X,type="l")

cox.stuart.test(X,alternative='right.sided')


# Série de Câmbio
y=read.table(file="EURUSD60.csv",sep=",")
y=data.frame(y)
names(y)= c("data","hora","abertura","alta","baixa","fechamento","volume")
attach(y)
plot(fechamento,type="l")       

cox.stuart.test(fechamento)

