
1) Testes de Bondade de ajuste

a) Teste Qui-quadrado de Ader?ncia

# Estat?stica: X^2=sum_{i=1}^{N}(Obser_i-Esper_i)^2/Esper_i=sum_{i=1}^{N}Observ_i^2/Esper_i - N

# X^2 (sob H0) ~ X^2_{k-1 g.l.}

# Frequencias esperadas devem ser >= 5

# Teste n?o deve ser usado se 20% das freq. esperadas s?o < 5 ou
# se qualquer frequencia esperada ? < 1

# Tabela C do Apendice

# Ex. 4.2a Siegel: N? de vit?rias de corridas de cavalo, por posi??o de largada
tab=matrix(0,nrow=2,ncol=8)
tab[1,]=c(29,19,18,25,17,10,15,11)
tab[2,]=rep(18,8)
tab

chisq.test(tab[1,])
sum((tab[1,]-tab[2,])^2/tab[2,])


## Vari?veis discretas, por?m com infinitos valores: agregar valores
#Exemplo: Poisson, classes ser?o [0,1,2,...,k,(k+1):infinito]
n=100
lambda=5
x=rpois(n,lambda)
hist(x)
# frequencia de cada valor
# vamos fazer classes = [0,1,...,9,10 ou +]   *minha amostra)
# calculo do valor esperado
p0=dpois(0:9,lambda)
p10=1-ppois(9,lambda)
p=c(p0,p10)
p
Esp=n*p
# montagem do valor observado
obs=as.matrix(table(x))
obs
# se frequencia de 0 (e 1) forem 0, concatenar 0 para estes valores
obsf=c(0,obs[1:9],obs[10])
obsf
x2=sum((Esp-obsf)^2/Esp)
x2
pvalor=1-pchisq(x2,length(Esp)-1)
pvalor

##########################################################################################################


b) Testes de Bondade de ajuste - KS (Kolmogorov-Smirnov) 

# Teste de Kolmogorov-Smirnov

# Estat?stica de Teste: D=max|F_0(X_i) - S_N(X_i)|

#No R, use ks.test
# Teste: ks.test(x,"dist"), onde dist: pnorm, pchi2, pgamma, ppois, etc...
# Exige a informa??o sobre os par?metros, a serem passados ap?s "pdist"

S_N: fun??o de distribui??o emp?rica;
F_0: distribui??o te?rica a ser testada

Abrir 
?ks.test

require(goftest)
?ad.test

# Exemplo 1 Conover, pag. 433

# H0: Os dados foram gerados de uma U(0,1)
# H1: Os dados n?o foram gerados de uma U(0,1)
x=c(0.621,0.503,0.203,0.477,0.710,0.581,0.329,0.480,0.554,0.382)
plot(ecdf(x))
x0=seq(0,1,by=0.1)
fda_teorica= x0
lines(x0,fda_teorica)
ks.test(x,"punif",0,1)  # ver tabela
ad.test(x,null="punif",0,1)

# Exemplo 2: Siegel, pag. 88 (experimento dos fios). X ~ U(0,6)

x=cbind(0.6, 0.8, 1.1, 1.2, 1.4, 1.7, 1.8, 1.9, 2.2, 2.4, 2.5, 2.9, 3.1, 3.4, 3.4, 3.9, 4.4, 4.9, 5.2, 5.9)

plot(ecdf(x))
x0=seq(0,6,by=0.1)
fda_teorica= x0/6
lines(x0,fda_teorica)     

ks.test(x,"punif",0,6)  # ver tabela
ad.test(x,null="punif",0,6)


# Exemplo 4.1 Gibbons

x=c(0.0123, 0.1039, 0.1954, 0.2621, 0.2802,0.3217, 0.3645, 0.3919, 0.4240, 0.4814,0.5139, 0.5846, 0.6275, 0.6541, 0.6889,0.7621, 0.8320, 0.8871, 0.9249, 0.9634)
xr=sqrt(x)

plot(ecdf(xr))
x0=seq(0,1,by=0.1)
fda_teorica= x0
lines(x0,fda_teorica)
ks.test(xr,"punif",0,1)  # ver tabela

plot(ecdf(x))            # Estranho ideia de Gibbons, porque a variavel original ? U(0,1)
x0=seq(0,1,by=0.1)
fda_teorica= x0
lines(x0,fda_teorica)
ks.test(x,"punif",0,1)   # ver tabela
ad.test(x,null="punif",0,1)

# Simulacao de X ~ F_X(theta_0)
# Exemplo:
n=20
x=rnorm(n,5,1.3)
xord=sort(x)
plot(ecdf(x))
x1=seq(min(x),max(x),by=0.01)
lines(x1,pnorm(x1))
#lines(xord,pnorm(xord))

ks.test(x,"pnorm",5,1.3)
ad.test(x,null="pnorm",5,1.3)

ks.test(x,"pnorm",mean(x),sd(x)) 
ad.test(x,null="pnorm",mean(x),sd(x))


#######################################################################################################################

c) Testes de Normalidade

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




 # Exemplo 1 Conover, pag. 444
 x=c(23,23,24,27,29,31,32,33,33,35,36,37,40,42,43,43,44,45,48,48,54,54,56,57,57,58,58,58,58,59,61,61,62,63,64,65,66,68,68,70,73,73,74,75,77,81,87,89,93,97)
 hist(x)
 # carregar library 'nortest'
require(nortest)
?lillie.test
 lillie.test(x)
 
# Conferindo com K-S
z=(x-mean(x))/sd(x)
ks.test(z,"pnorm") 

# Estat?stica igual, p_valor diferente

# Outro teste de NORMALIDADE:
?shapiro.test
shapiro.test(x)
dagostino.pearson.test(x)
# Minha ideia: Z ~ t(n-1)
nu= length(z)-1
ks.test(z,"pt",nu)  
   

############################# Compara??o entre testes ###########################
# H0
x=rnorm(100,5,1) 
hist(x)
lillie.test(x)
shapiro.test(x)
dagostino.pearson.test(x)
ad.test(x)
cvm.test(x)

# Assimetria
require(sn)
x=rsn(5000,5,1,1)  # variar lambda's
hist(x)
lillie.test(x)
shapiro.test(x)
dagostino.pearson.test(x)
ad.test(x)
cvm.test(x)

# Misturas de Normais -> Bimodalidade
x1=rnorm(100,0,1)
x2=rnorm(100,4,1)
x=c(x1,x2)
hist(x)
lillie.test(x)
shapiro.test(x)
dagostino.pearson.test(x)
ad.test(x)
cvm.test(x)

# Achatamento/curtose
x=rt(200,3)
hist(x,nclass=15)
lillie.test(x)
shapiro.test(x)
dagostino.pearson.test(x)
ad.test(x)

##### Fazendo 1000 vezes e calcular a proporção de rejeicao
replic=10000
pvl=rep(0,replic)
pvs=pvl
pvd=pvs
pvad=pvs
for (i in 1:replic){
    x=rnorm(100,5,1) 
    a=lillie.test(x)
    b=shapiro.test(x)
    c=dagostino.pearson.test(x)
    d=ad.test(x)
    pvl[i]=a$p.value
    pvs[i]=b$p.value
    pvd[i]=c$p.value
    pvad[i]=d$p.value
}

mean(pvl<0.05)
mean(pvs<0.05)
mean(pvd<0.05)
mean(pvad<0.05)

# Teste de Lilliefors para a distrib. Exponencial
x=rexp(100,5)
hist(x)
z=x/mean(x)
aa=ecdf(z)
plot(ecdf(z))
x1=seq(0,7,by=0.01)
lines(x1,pexp(x1))

# Usar tabela KS
alpha=5%
ks=ks.test(x/mean(x),"pexp",1) # s? pra pegar Dn
Dn=ks$statistic
d=sqrt(n)*Dn
d
vc=1.36

###################################################################################################################
###################################################################################################################
###################################################################################################################

2) Testes de simetria

# Teste ? assint?tico (amostra grande), usa aproxima??o pela normal

# carregar library 'lawstat'

symmetry.test(x, option = "opt")

# opt (uma dessas): mgg.test", "cabilio.masaro.test", "mira.test"

# Estat?stica ? comparada com N(0,1)
?symmetry.test
x=rnorm(100)
hist(x)
boxplot(x)
library(lawstat)
symmetry.test(x,option="MGG")
symmetry.test(x,option="CM")
symmetry.test(x,option="M")

library(sn)
library(nortest)
x=rsn(100,5,1,7)  # mu, sigma, lambda
hist(x)
symmetry.test(x,option="MGG")
symmetry.test(x,option="CM")
symmetry.test(x,option="M")

lillie.test(x)
shapiro.test(x)


x=rsn(100,5,1,3)  # mu, sigma, lambda
hist(x)

symmetry.test(x,option="mgg.test")
symmetry.test(x,option="cabilio.masaro.test")
symmetry.test(x,option="mira.test")

lillie.test(x)
shapiro.test(x)

x=rsn(10000,5,1,1)  # mu, sigma, lambda
hist(x)

symmetry.test(x,option="MGG")
symmetry.test(x,option="CM")
symmetry.test(x,option="M")

lillie.test(x)
shapiro.test(x)
##################################################
x=rt(100,1)  # mu, sigma, lambda
hist(x)

symmetry.test(x,option="mgg.test")
symmetry.test(x,option="cabilio.masaro.test")
symmetry.test(x,option="mira.test")

lillie.test(x)
shapiro.test(x)


########################################################################################################################
########################################################################################################################
########################################################################################################################

3) Testes de Aleatoriedade

a) Teste das corridas: para vari?veis bin?rias e cont?nuas

# Para vari?veis bin?rias: N tamanho da amostra, n1: n? de '1', n2: n? de '0', n=n1+n2
# Para vari?veis cont?nuas: '+' se X_i > Mediana(X), '-', caso contr?rio
# Estat?stica de teste: r= n? de s?ries
# Teste exato (pequenas amostras): Use a fun??o 'densRuns' para calcular a fun??o de probabilidade.
# Castellan usa m, n e N. Gibbons usa n1, n2 e n; ? a mesma coisa
# Use a Tabela G (Siegel) do Apendice: Intervalos de Confian?a
# Ou fa?a os c?lculos usando a fun??o densRuns

# C?lculo do n? de s?ries para var. bin?rias
NumSeriesDisc <-function(y){
# y deve ser um vetor de 0's e 1's
n1=sum(y)
n2=length(y)-n1
series=1
for (i in 2:length(y)){
if(y[i]!=y[i-1]){series<-series+1}}
r=series
print(c("n1", "n2", "n? de s?ries"))
return(c(n1,n2,r))
}

# C?lculo do n? de s?ries para var. cont?nuas (usando mediana)
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
print(grupo)
print(c("n1", "n2", "n? de s?ries"))
return(c(n1,n2,r))
}

# Teste das Corridas para grande amostras (aproxima??o pela normal)
RunsTesteAprox <- function(y,n1,n2,r){
# hc: ccorre??o de continuidade
n=n1+n2
media=2*n1*n2/n+1
varR=2*n1*n2*(2*n1*n2-n)/(n^2*(n-1))
if (r>media) hc=-1/2 else hc=1/2
rz=(r+hc-media)/sqrt(varR)
p_valor=2*pnorm(-abs(rz))
print("p_valor")
return(p_valor)
}

# Fun??o para calcular probabilidades de s?ries (r)
densRuns <-function(n1,n2,r){
n=n1+n2
# r: n? de s?ries ou corridas
if (r/2>as.integer(r/2)) fr=(choose(n1-1,(r-1)/2)*choose(n2-1,(r-3)/2)+choose(n1-1,(r-3)/2)*choose(n2-1,(r-1)/2))/choose(n,n1)
else fr=2*choose(n1-1,r/2-1)*choose(n2-1,r/2-1)/choose(n,n1)
return(fr)
}

cdfRuns <- function(n1,n2,qe){
#qe >=2
p=0
for (r in 2:qe) p = p+densRuns(n1,n2,r)
return(p)
}

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
# validar com tabela G Siegel
qRuns(10,10,0.025)
qRuns(10,10,0.975)

############################

# Exemplos:
# n1=5 e n2=4
densRuns(5,4,2)
densRuns(5,4,3)
densRuns(5,4,9)
densRuns(5,4,8)

# var. cont?nuas
library(lawstat)
?runs.test
y = arima.sim(n = 40, list(ar = c(0.5)))
plot(y)
runs.test(y)
# Note que R faz apenas o teste assint?tico (aprox. pela normal)
# N?o faz corre??o de continuidade!
R=NumSeriesCont(y)
R
n1=R[1]
n2=R[2]
r=R[3]
qRuns(n1,n2,0.025)
qRuns(n1,n2,0.975)

# s? para ver se ? pr?ximo do runs.test
RunsTesteAprox(y,n1,n2,r)

# Ex.: Var. Bin?rias
y=rbinom(15,1,0.4)
plot(y,type="l")
R=NumSeriesDisc(y)
R
n1=R[1]
n2=R[2]
qRuns(n1,n2,0.025)
qRuns(n1,n2,0.975)

y=rbinom(100,1,0.4)
plot(y,type="l")
R=NumSeriesDisc(y)
R
n1=R[1]
n2=R[2]
n=n1+n2
r=R[3]
ER=1+2*n1*n2/n
VarR=2*n1*n2*(2*n1*n2-1)/(n^2*(n-1))
z=(r-ER-0.5*sign(r-ER))/sqrt(VarR)
z
p_valor=2*pnorm(-abs(z))
p_valor

y=sort(rnorm(20))
R=NumSeriesCont(y)
R
n1=R[1]
n2=R[2]
r=R[3]
qRuns(n1,n2,0.025)
qRuns(n1,n2,0.975)


# Amostras Grandes
y = arima.sim(n = 100, list(ar = c(0.5)))
plot(y)
runs.test(y)
# Note que R faz apenas o teste assint?tico (aprox. pela normal)
r=NumSeriesCont(y)
r
RunsTesteAprox(y,r[1],r[2],r[3])

y=rnorm(100)
plot(y)
runs.test(y)
bartels.test(y)
r=NumSeriesCont(y)
r
RunsTesteAprox(y,r[1],r[2],r[3])


y=rnorm(1000)
runs.test(y)
r=NumSeriesCont(y)
r
RunsTesteAprox(y,r[1],r[2],r[3])

y=rbinom(100,1,0.5)
plot(y,type="l")
runs.test(y)     # n?o pode ser utilizada para var. bin?rias!!!
r=NumSeriesDisc(y)
r
RunsTesteAprox(y,r[1],r[2],r[3])

# Notem que a ideia n?o ? de que os valores s?o uniformes.
# Eles podem ser maiores ou menores que a m?dia.
# Aleatoriedade quer dizer que a ordem em que as observa??es aparecem ? que ? aleat?ria


 
b) Teste de RVN de Bartels (var. cont?nuas)

# Pequenas amostras

x=rnorm(50)
N=length(x)
R=rank(x, na.last = TRUE, ties.method = "average")
R2=R[2:N]
R1=R[1:N-1]
NM=sum((R2-R1)^2)
DM=sum((R-mean(R))^2)

RVN=NM/DM
RVN

## Amostra grande

Ervn=2
Vrvn=4*(N-2)*(5*N^2-2*N-9)/(5*N*(N+1)*(N-1)^2)
RVNpad=(RVN-Ervn)/sqrt(Vrvn)
RVNpad

R fornece somente teste padronizado (amostras grandes)
library(lawstat)
bartels.test(x)

bartels.test(y, alternative = c("two.sided", "positive.correlated",
             "negative.correlated"))

# n?o bate com RVNpad
# teste com Vari?ncia aproximada
Ervn=2
Vrvn=20/(5*N+7)
RVNpad=(RVN-Ervn)/sqrt(Vrvn)
RVNpad
# tbm n?o bate 
# Abrir fun??o do R
 

statistic <- (RVN - 2) * sqrt(T)/2 # T=n (aqui est? a diferen?a: na var. aproximada)

x=rnorm(200)
aa=bartels.test(x)
RVN=aa$parameter
N=length(x)
Vrvn=4*(N-2)*(5*N^2-2*N-9)/(5*N*(N+1)*(N-1)^2)
RVNpad=(RVN-Ervn)/sqrt(Vrvn)
RVNpad # j? fica bem parecido
 

########################################################################################################################
########################################################################################################################
########################################################################################################################

4) Teste de ponto de mudan?a

## para vari?veis bin?rias e cont?nuas

## x ? bin?ria (0 ou 1)
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
print(c("Estat. Dmn","ponto mudan?a"))
return(c(Dcal,pm))
}

Dmn_calc=TestPMDisc(x)
length(x)
sum(x)

Dmn_calc

# Estat?stica de Teste: D_m,n (p?g. 85)
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


## Cont?nuas

N=20
x=rnorm(N)
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

x=rnorm(N)
TestePMCont(x)
m=35
sum(rank(x[1:m]))

# Teste exato: Use Tabela J (Siegel)
# Se Kmn > W tabelado, rejeite H0 (h? mudan?a na distribui??o)
# Para usar a tabela J: calcule a soma dos postos at? a posi??o m e compare com a tabela


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
print(c("pto mudan?a","Wz","p_valor"))
return(c(m,Wz,p_valor))
}
# Exemplos
y=rnorm(100)
plot(y,type="l")
TestePMContaprox(y)

y = arima.sim(n = 100, list(ar = c(0.5, -0.5), ma = c(-1, 5)), sd = sqrt(0.1796))
plot(y,type="l")
TestePMContaprox(y)

# Exemplo: Dolar x Euro, de hora em hora
# Detectar quando existir uma mudan?a bruta
# colunas representam, respectivamente: DATE (data), TIME (hora), OPEN (abertura), HIGH (alta), LOW (baixa), CLOSE (fechamento) e VOLUME (n?mero de pessoas que compraram/venderam as referidas moedas).
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


########################################################################################################################
########################################################################################################################
########################################################################################################################


6) Teste de Tend?ncia de Cox & Stuart
# Ex. 2 - Conover, p?g. 171
X=c(45.25,45.83,41.77,36.26,45.37,52.25,35.37,57.16,35.37,58.32,41.05,33.72,45.73,37.90,41.72,36.07,49.83,36.24,39.90)
n=length(X)

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
D=parX(X)
plot(D,type="l")
T=length(D[D>0])
T

# Hip?tese bilateral, n pequeno
alpha=0.05
n=length(D)
binom.test(T,n,1/2)

# Ex. 3 - Conover, p?g. 171/172
X=c(14.6,12.2,104,220,110,86,92.8,74.4,75.4,51.7,29.3,16,14.2,10.5,123,190,138,98.1,88.1,80,75.6,48.8,27.1,15.7)
D=parX(X)
plot(D,type="l")
T=length(D[D>0])
T

# H1: a taxa de descarga est? decrescendo
alpha=0.1
n=length(D)
binom.test(T,n,1/2,alter="l")


# Ex. 5 Conover, p?g. 173/174
X=c(84,60,63,109,111,83,146,166,119,143,151,116,137,163,139,208,174,283,176,296,176,286,223,235)
plot(X,type="l")
D=parX(X)
plot(D,type="l")
T=length(D[D>0])
n=length(D)
T
alpha=0.05
binom.test(T,n,1/2,alternative="g")

# S?rie de C?mbio
y=read.table(file="EURUSD60.csv",sep=",")
y=data.frame(y)
names(y)= c("data","hora","abertura","alta","baixa","fechamento","volume")
attach(y)
plot(fechamento,type="l")

D=parX(fechamento)
plot(D)
T=length(D[D>0])
T

# Hip?tese bilateral, n pequeno
alpha=0.05
n=length(D)
pbinom(T,n,1/2)
