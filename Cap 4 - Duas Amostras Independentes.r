 Cap. 4  - Duas Amostras Independentes


1) Testes de Aderência

H1: A FDA de X vive abaixo da FDA de Y
H1: X é estocasticamente MENOR que Y

X=rnorm(100,2,1)
Y=rnorm(100,3,1)   

xmin=min(c(X,Y))
xmax=max(c(X,Y))
Xp=density(X,from=xmin,to=xmax) #  estima uma densidade não-paramétrica
Yp=density(Y,from=xmin,to=xmax)
aplt=c(xmin,xmax,0,max(c(Xp$y,Yp$y)))
plot(Yp$x,Yp$y,type="l",col="blue",xlab="",ylab="",plt=aplt,xlim=c(xmin,xmax),ylim=c(0,max(c(Xp$y,Yp$y))))
points(Y,rep(0,length(X)),col="blue")  
lines(Xp$x,Xp$y,type="l",col="red",xlab="",ylab="",plt=aplt)
points(X,rep(0,length(Y)),col="red")
# novo gráfico
plot(ecdf(X),xlim=c(xmin,xmax))
plot(ecdf(Y),add=T,pch=2)


# dados podem ser 


1.1 Teste K-S

?ks.test

# Exemplo 6.6a Siegel
serie2=c(35.2,39.2,40.9,38.1,34.4,29.1,41.8,24.3,32.4)
serie7=c(39.1,41.2,45.2,46.2,48.4,48.7,55.0,40.6,52.1,47.2)

plot(ecdf(serie2),xlim=c(min(c(serie2,serie7)),max(c(serie2,serie7))))
plot(ecdf(serie7),add=T,lty=3,pch=17,col='blue')

aa=ks.test(serie2,serie7,alternative="g") 
aa
ds=as.numeric(aa$statistic)
ds2=length(serie2)*length(serie7)*ds
ds2

ks.test(serie2,serie7,alternative="l")

# Para m, n <=25, Pegue o valor de D, calcule m*n*D e vá para a Tabela LI (unilateral) ou LII (bilateral)
# Grandes amostras -> Tabela LIII do Siegel  (mas aí já pode usar o p-valor do output da funçao, sem precisar recorrer à tabela)

# Hipóteses unilaterais
# FX < FY (X é estocastimente MAIOR que Y)
# se vc entra ks.test(X,Y) então ele está testando FX-FY

# para 'pegar' o valor de Dmn
ks=ks.test(x,y,alternative="g")
ks
D=ks$statistic
X2=4*(D^2)*m*n/(m+n)
p_valor=1-pchisq(X2,2)  # para grandes amostras, pode usar o p-valor do output da funcao ks.test
p_valor

################ Grandes Amostras:
m=30
n=40
x=rnorm(m,8.5,1)
y=rnorm(n,8,1)

plot(ecdf(x),xlim=c(min(c(x,y)),max(c(x,y))))
plot(ecdf(y),add=T,pch=17,col='blue')

hist(y,xlim=c(min(c(x,y)),max(c(x,y))),col="green")
hist(x,add=T,col="pink") 




1.2 Cramér-Von Mises

i. estatistica notas de aula e pdf funcao do pacote coda
require(coda)
?pcramer # gives the distribution function
 
Cramer.estat <-function(X,Y){
n=length(X)
m=length(Y)
Sx=ecdf(X)
Sy=ecdf(Y)
SxX=Sx(X)
SxY=Sx(Y)
SyX=Sy(X)
SyY=Sy(Y)
T2=m*n/((m+n)^2)*(sum((SxX-SyX)^2)+sum((SxY-SyY)^2))
return(T2)
}

T2=Cramer.estat(X,Y)
pv=1-pcramer(T2, 1e-5)
pv 

ii. Pacote cramer
require(cramer)
cramer.test(X,Y) 
 
# Ex. 2 Conover, pág. 464
prec=1e-6
X=c(7.6,8.4,8.6,8.7,9.3,9.9,10.1,10.6,11.2)
Y=c(5.2,5.7,5.9,6.5,6.8,8.2,9.1,9.8,10.8,11.3,11.5,12.3,12.5,13.4,14.6)

# Testes                       
ks.test(X,Y)
T2=Cramer.estat(X,Y)
T2 # igual Conover, pag. 464
pvalor=1-pcramer(T2,prec)
pvalor  # concorda com Conover
cramer.test(X,Y)



######################## Exemplos
prec=1e-6
X=rnorm(30,8.5,1)
Y=rnorm(40,8,1)              
ks.test(X,Y)
T2=Cramer.estat(X,Y)
T2 
pvalor=1-pcramer(T2,prec)
pvalor  
cramer.test(X,Y)

X=rnorm(30,8.5,1)
Y=8+rt(40,5)              
ks.test(X,Y)
T2=Cramer.estat(X,Y)
T2 
pvalor=1-pcramer(T2,prec)
pvalor
cramer.test(X,Y)

require(sn)
X=as.numeric(rsn(30,8.5,1,3))
Y=as.numeric(rsn(40,8,1,-1))              
ks.test(X,Y)
T2=Cramer.estat(X,Y)
T2 
pvalor=1-pcramer(T2,prec)
pvalor  
cramer.test(X,Y)




##########################################################################################################################
##########################################################################################################################
##########################################################################################################################

2) Teste para Locação 

2.1 Teste da Mediana

# Ex. 6.3 (Siegel), Tabela 6.17
H0: Não há diferença na mediana(Ler no livro)
H1: mediana da ansiedade de sociedades com explanações orais PRESENTES é maior que nas AUSENTES

# Dados já em tabela (com estatísticas de mediana já calculados)
tab7=matrix(c(3,13,17,6),nrow=2,ncol=2,dimnames = list(c("Sociedades acima da mediana em ansiedade de socialização da oralidade", "Sociedades abaixo da mediana em ansiedade de socialização da oralidade"),c("Sociedades com explanação oral ausente", "Sociedades com explanação oral presente")))
chisq.test(tab7)


# Teste Conover
median.testConover <- function(X,Y){
m=length(X)
n=length(Y)
N=m+n
AmComb=c(X,Y)
medAC=median(AmComb)
O11=sum(X>medAC)
O12=sum(Y>medAC)
a=O11+O12
b=N-a

test=N^2/(a*b)*(O11^2/m+O12^2/n) - N*a/b
pv=1-pchisq(test,1)
tab=matrix(c(test,pv),1,2)
colnames(tab)=c('Estatística','p-valor')
return(tab) 
}


median.test2sample <- function(X,Y){
AmComb=c(X,Y)
medAC=median(AmComb)
A=sum(X>medAC)
B=sum(Y>medAC)
m=length(X)
n=length(Y)
C=m-A
D=n-B
tabMed=matrix(c(A,C,B,D),2,2)
tabMed
stab=sum(tabMed)
if (stab< 20) test= fisher.test(tabMed)
else test=chisq.test(tabMed)
return(test) 
}



#### library coin
require(coin)

diffusion <- data.frame(
    pd = c(0.80, 0.83, 1.89, 1.04, 1.45, 1.38, 1.91, 1.64, 0.73, 1.46,
           1.15, 0.88, 0.90, 0.74, 1.21),
    age = factor(rep(c("At term", "12-26 Weeks"), c(10, 5)))
)

median_test(formula, data, subset = NULL, weights = NULL, ...)
## S3 method for class 'IndependenceProblem'
median_test(object, mid.score = c("0", "0.5", "1"), conf.int = FALSE, conf.level = 0.95, ...)


# Exemplos
X=rnorm(30,5,1)
Y=rnorm(30,5,1)
median.testConover(X,Y)   # diferente dos outros dois
median.test2sample(X,Y)

ex <- data.frame(
    dados = c(X,Y),
    trat = factor(rep(c("X", "Y"), c(length(X), length(Y))))
)
median_test(dados ~ trat, data = ex, distribution = "exact")


X=rnorm(30,5,1)
Y=rnorm(30,6,1)
median.testConover(X,Y) 
median.test2sample(X,Y)

ex <- data.frame(
    dados = c(X,Y),
    trat = factor(rep(c("X", "Y"), c(length(X), length(Y))))
)
median_test(dados ~ trat, data = ex, distribution = "exact")


Y=rnorm(40,7,3)
median.test2sample(X,Y)
ex <- data.frame(
    dados = c(X,Y),
    trat = factor(rep(c("X", "Y"), c(length(X), length(Y))))
)
median_test(dados ~ trat, data = ex, distribution = "exact")


Y=5+1*rt(100,3)
median.test2sample(X,Y)
ex <- data.frame(
    dados = c(X,Y),
    trat = factor(rep(c("X", "Y"), c(length(X), length(Y))))
)
median_test(dados ~ trat, data = ex, distribution = "exact")


library(sn)
Y=rsn(50,5,1,3)
median.test2sample(X,Y)
ex <- data.frame(
    dados = c(X,Y),
    trat = factor(rep(c("X", "Y"), c(length(X), length(Y))))
)
median_test(dados ~ trat, data = ex, distribution = "exact")


2.2 Teste de Wilcoxon-Mann-Whitney (WMW)
WMW testa se X e Y tem a MESMA distribuição. 
Sua melhor utilidade é em testar se amostras X e Y foram extraídas da mesma população.
Observação: o teste de WMW pode ser usado para testar medianas, porém fica mais fraco.


wilcox.test(X,Y)    # stats; teste uni e bilateral
wilcox.exact(X,Y)  # exactRankTests, mesmo resultado

X=c(9,11,15)
Y=c(6,8,10,13)
wilcox.test(X,Y)    # stats


2.3  Teste de van der Waerden
require(agricolae)

dados = c(X,Y)
trat = factor(rep(c("X", "Y"), c(length(X), length(Y))))  
fit=waerden.test(dados, trat, alpha=0.05)
fit

ex <- data.frame(
    dados = c(X,Y),
    trat = factor(rep(c("X", "Y"), c(length(X), length(Y))))
)
out1<-with(ex,waerden.test(dados,trat,group=TRUE))

# 
require(coin)
ex <- data.frame(
    dados = c(X,Y),
    trat = factor(rep(c("X", "Y"), c(length(X), length(Y)))))
normal_test(dados ~ trat, data = ex, distribution = "exact")



2.4 Teste de Posto-Ordem Robusto


POR.test <-function(X,Y){
X=sort(X)
Y=sort(Y)
M=length(X)
UYX=rep(0,M)
for (j in 1:M) UYX[j]=sum(Y<X[j])  
N=length(Y)
UXY=rep(0,N)
for (j in 1:N) UXY[j]=sum(X<Y[j])
UYXm=mean(UYX)
UXYm=mean(UXY)        
Vx=sum((UYX-UYXm)^2)
Vy=sum((UXY-UXYm)^2)  
U=(M*UYXm-N*UXYm)/(2*sqrt(Vx+Vy+UXYm*UYXm))
return(U)}

X=c(9,11,15)
Y=c(6,8,10,13)
por=POR.test(X,Y)
por

# Novidade
require(RVAideMemoire)
# fp.test(x, y, delta = 0, alternative = "two.sided", ...)
fp.test(Y,X)   # entradas de X,Y ao contrario da minha funcao

# outro pacote - NSM3, do livro do Hollander (2014), mas muito pesado
require(NSM3)       # dah problema em todos, pequenas amostras
?cFligPoli 
alphae=0.025
alphad=0.0975
m=length(X)
n=length(Y)
cFligPoli(alphae,m,n,method=NA)
cFligPoli(alphad,m,n,method=NA)

cFligPoli(alphae,m,n,method='Exact')
cFligPoli(alphad,m,n,method='Exact')

cFligPoli(alphae,m,n,method='Monte Carlo')
cFligPoli(alphad,m,n,method='Monte Carlo')

cFligPoli(alphae,m,n,method='Asymptotic')
cFligPoli(alphad,m,n,method='Asymptotic')     ############# NÃO USAR ###########


2.5. Teste da Permutação  - Seção 6.7 Siegel ou Conover, p. 409 (Teste da Aleatorização)
# Testa a diferença entre duas médias
# tamanhos das amostras pequenos
library(coin) 

# entrada dos dados
ex <- data.frame(
    dados = c(X,Y),
    trat = factor(rep(c("X", "Y"), c(length(X), length(Y)))))
oneway_test(dados ~ trat, data = ex, distribution = "exact")

require(exactRankTests)
perm.test(X,Y) # uni e bilateral

##################################### Exemplos  ################################################
# Aplicar Testes: Mediana, WMW, van der Waerden, Posto-Ordem Robusto e Permutacao/Aleatorizacao


# Ex. 1
X=c(110,70,53,51)
Y=c(78,64,75,45,82)

ex <- data.frame(
    dados = c(X,Y),
    trat = factor(rep(c("X", "Y"), c(length(X), length(Y)))))


median.test2sample(X,Y)   # Mediana
wilcox.test(X,Y)          # WMW
normal_test(dados ~ trat, data = ex, distribution = "exact")    # Waerden
fp.test(Y,X)                                                    # POR  # entradas de X,Y ao contrario da minha funcao
oneway_test(dados ~ trat, data = ex, distribution = "exact")    # Permutacao/aleatorizacao


#Ex. 2
X=rnorm(30,5,1)       # H0 falsa
Y=rnorm(30,7,2)
ex <- data.frame(
    dados = c(X,Y),
    trat = factor(rep(c("X", "Y"), c(length(X), length(Y)))))  

median.test2sample(X,Y)   # Mediana
wilcox.test(X,Y)          # WMW
normal_test(dados ~ trat, data = ex)#, distribution = "exact")    # Waerden
fp.test(Y,X)                                                    # POR  # entradas de X,Y ao contrario da minha funcao
oneway_test(dados ~ trat, data = ex)#, distribution = "exact")    # Permutacao/aleatorizacao


X=rnorm(30,5,1)      # H0 verdadeira
Y=5+1*rt(30,3)
ex <- data.frame(
    dados = c(X,Y),
    trat = factor(rep(c("X", "Y"), c(length(X), length(Y)))))
    
median.test2sample(X,Y)   # Mediana
wilcox.test(X,Y)          # WMW
normal_test(dados ~ trat, data = ex)#, distribution = "exact")    # Waerden
fp.test(Y,X)                                                    # POR  # entradas de X,Y ao contrario da minha funcao POR.test
oneway_test(dados ~ trat, data = ex)#, distribution = "exact")    # Permutacao/aleatorizacao


# Ex.3: Siegel, 6.4.b
# X: Sociedade com explanação oral AUSENTE
# Y: Sociedade com explanação oral PRESENTE
X=c(13,12,12,10,10,10,10,9,8,8,7,7,7,7,7,6)
Y=c(17,16,15,15,15,14,14,14,13,13,13,12,12,12,12,11,11,10,10,10,8,8,6)
ex <- data.frame(
    dados = c(X,Y),
    trat = factor(rep(c("X", "Y"), c(length(X), length(Y)))))
    
median.test2sample(X,Y)   # Mediana
wilcox.test(X,Y)          # WMW
normal_test(dados ~ trat, data = ex, distribution = "exact")    # Waerden
fp.test(Y,X)                                                    # POR  # entradas de X,Y ao contrario da minha funcao
oneway_test(dados ~ trat, data = ex, distribution = "exact")    # Permutacao/aleatorizacao


# Ex. 4, pág. 276 do Conover (Exemplo 1)
# X: Farm Boys
# Y: Town Boys
# Menor score -> pior condição física
# H0: Farm boys do not tend to be more fit, physically, than town boys
# E(X) <= E(Y)
# H1: Farm boys tend to be more fit than town boys 
# E(X) > E(Y)
X=c(14.8,7.3,5.6,6.3,9,4.2,10.6,12.5,12.9,16.1,11.4,2.7)
Y=c(12.7,14.2,12.6,2.1,17.7,11.8,16.9,7.9,16,10.6,5.6,5.6,7.6,11.3,8.3,6.7,3.6,1,2.4,6.4,9.1,6.7,18.6,3.2,6.2,6.1,15.3,10.6,1.8,5.9,9.9,10.6,14.8,5,2.6,4)
ex <- data.frame(
    dados = c(X,Y),
    trat = factor(rep(c("X", "Y"), c(length(X), length(Y)))))
    
#median.test2sample(X,Y)   # Mediana
wilcox.test(X,Y,alternative = "greater")          # WMW
normal_test(dados ~ trat, data = ex,alternative = "greater")#, distribution = "exact")    # Waerden
fp.test(Y,X,alternative = "greater")                                                    # POR  # entradas de X,Y ao contrario da minha funcao
oneway_test(dados ~ trat, data = ex,alternative = "greater")#, distribution = "exact")    # Permutacao/aleatorizacao



# dados do Ciro:  Teste unilateral
X=scan(file="DadosCiro1.txt")
Y=scan(file="DadosCiro2.txt")
ex <- data.frame(
    dados = c(X,Y),
    trat = factor(rep(c("X", "Y"), c(length(X), length(Y)))))
# H0: E(X)=E(Y) x H1: E(X)>E(Y)
# median.test2sample(X,Y) # não pode ser aplicado em testes unilaterais
wilcox.test(X,Y,alternative="gr")
perm.test(X,Y,alternative="gr")
normal_test(dados ~ trat, data = ex, alternative = "greater")  # dah problema de espaco para amostras maiores, mudar exact
fp.test(Y,X,alternative = "greater")   # entradas de X,Y ao contrario da minha funcao
oneway_test(dados ~ trat, data = ex, alternative = "greater")  # dah problema de espaco para amostras maiores, mudar exact

# Exemplo 6.5 (Siegel)
# X: nível de atividade de dopamina...entre não-psicóticos
# Y: nível de atividade de dopamina...entre psicóticos
# H0: E(X) = E(Y) x H1: E(X)#E(Y)
X=c(0.0252,0.0230,0.021,0.02,0.02,0.018,0.017,0.0156,0.0154,0.0145,0.013,0.0116,0.0112,0.0105,0.0104)
Y=c(0.032,0.0306,0.0275,0.027,0.0245,0.0226,0.0222,0.0208,0.0204,0.015)
ex <- data.frame(
    dados = c(X,Y),
    trat = factor(rep(c("X", "Y"), c(length(X), length(Y)))))


median.test2sample(X,Y)   # Mediana
wilcox.test(X,Y)          # WMW
normal_test(dados ~ trat, data = ex, distribution = "exact")    # Waerden
fp.test(Y,X)                                                    # POR  # entradas de X,Y ao contrario da minha funcao
oneway_test(dados ~ trat, data = ex, distribution = "exact")    # Permutacao/aleatorizacao



### Simulando poder dos testes

n1=500
n2=500                          
K=1000
pv_med=rep(0,K)
pv_wilcox=rep(0,K)
pv_waerden=rep(0,K)
pv_por=rep(0,K)
pv_perm=rep(0,K)
pv_t=rep(0,K)
for (k in 1:K){

X=rnorm(n1)     
Y=rnorm(n2)
auxN=sqrt(n1*n2*(n1+n2-2)/(n1+n2))

aa=median.test2sample(X,Y)   # Mediana
pv_med[k]=aa$p.value
aa=wilcox.test(X,Y)          # WMW
pv_wilcox[k]=aa$p.value
ex <- data.frame(
    dados = c(X,Y),
    trat = factor(rep(c("X", "Y"), c(length(X), length(Y)))))
aa<-with(ex,waerden.test(dados,trat,group=TRUE))  # Waerden
pv_waerden[k]=as.numeric(aa$statistics[3])
aa=fp.test(Y,X)  
pv_por[k]=aa$p.value      # POR  
aa=perm.test(X,Y)
pv_perm[k]=aa$p.value
est_t=(mean(X)-mean(Y))*auxN/sqrt((n1-1)*sd(X)+(n2-2)*sd(Y))
pv_t[k]=2*pnorm(-abs(est_t))
}
AA = matrix(0,K,5)
AA[,1]=pv_med
AA[,2]=pv_wilcox
AA[,3]=pv_waerden
AA[,4]=pv_perm
AA[,5]=pv_t
# Aproxima de alpha sob H0 e cresce se H1 fica longe de 0
mean(pv_med<0.05)
mean(pv_wilcox<0.05)
mean(pv_waerden<0.05)
mean(pv_perm<0.05)
mean(pv_t<0.05)

# demora um pouco
> mean(pv_med<0.05)
[1] 0.069
> mean(pv_wilcox<0.05)
[1] 0.058
> mean(pv_waerden<0.05)
[1] 1
> mean(pv_perm<0.05)
[1] 0.052
> mean(pv_t<0.05)
[1] 0.052


 # H0 falsa
n1=100
n2=100  
auxN=sqrt(n1*n2*(n1+n2-2)/(n1+n2))                        
K=1000
pv_med=rep(0,K)
pv_wilcox=rep(0,K)
pv_waerden=rep(0,K)
pv_por=rep(0,K)
pv_perm=rep(0,K)
pv_t=rep(0,K)
for (k in 1:K){

#X=rnorm(n1)   
#Y=rnorm(n2,1,1)   # H0 falsa
#X=rbeta(n1,3,2)  
#Y=rchisq(n2,3)
# simetricas
X=rt(n1,3)
Y=0.5+rt(n2,3)   # maiores diferencas nas medias podem variar a intensidade dos p-valores dos testes
# assimetricas
X=rsn(n1,0,1,3)
Y=rsn(n2,1,1,3)  # variancias iguais

aa=median.test2sample(X,Y)   # Mediana
pv_med[k]=aa$p.value
aa=wilcox.test(X,Y)          # WMW
pv_wilcox[k]=aa$p.value
#aa=normal_test(dados ~ trat, data = ex, distribution = "exact")    # nao consigo extrair p-valor
aa<-with(ex,waerden.test(dados,trat,group=TRUE))  # Waerden
pv_waerden[k]=as.numeric(aa$statistics[3])
aa=fp.test(Y,X)  
pv_por[k]=aa$p.value      # POR  
aa=perm.test(X,Y)
pv_perm[k]=aa$p.value
est_t=(mean(X)-mean(Y))*auxN/sqrt((n1-1)*sd(X)+(n2-2)*sd(Y))
pv_t[k]=1#2*pnorm(-abs(est_t))
}
AA = matrix(0,K,6)
AA[,1]=pv_med
AA[,2]=pv_wilcox
AA[,3]=pv_waerden
AA[,4]=pv_por
AA[,5]=pv_perm
AA[,6]=pv_t

AA1 = matrix(0,K,6)
for (k in 1:K){AA1[k,]=AA[k,]<=min(AA[k,])}
#AA1
colSums(AA1)





##########################################################################################################################
##########################################################################################################################
##########################################################################################################################

3) Teste para Diferenças de Escalas 

3.1 Teste de Mood

X=c(9,11,15)
Y=c(6,8,10,13)

mood.test(X,Y)  #stats

require(coin)
ex <- data.frame(
    dados = c(X,Y),
    trat = factor(rep(c("X", "Y"), c(length(X), length(Y)))))

mood_test(dados ~ trat, data = ex, distribution = "exact")



3.2 Teste de escores Normais de Klotz

ex <- data.frame(
    dados = c(X,Y),
    trat = factor(rep(c("X", "Y"), c(length(X), length(Y)))))

klotz_test(dados ~ trat, data = ex, distribution = "exact")



3.3 Teste de Siegel-Tukey para Diferenças de Escalas
siegel.tukey <- function(x, y, id.col = FALSE, adjust.median = F, 
    rnd = -1, alternative = "two.sided", mu = 0, paired = FALSE, 
    exact = FALSE, correct = TRUE, conf.int = FALSE, conf.level = 0.95) {
    ###### published on:
    #   http://www.r-statistics.com/2010/02/siegel-tukey-a-non-parametric-test-for-equality-in-variability-r-code/
    ## Main author of the function:  Daniel Malter
    
    # x: a vector of data
    
    # y: Group indicator (if id.col=TRUE); data of the second group (if id.col=FALSE). If y is the group indicator it MUST take 0
    #   or 1 to indicate the groups, and x must contain the data for both groups.
    
    # id.col: If TRUE (default), then x is the data column and y is the ID column, indicating the groups. If FALSE, x and y are both data
    #   columns. id.col must be FALSE only if both data columns are of the same length.
    
    # adjust.median: Should between-group differences in medians be leveled before performing the test? In certain cases, the Siegel-Tukey
    #   test is susceptible to median differences and may indicate significant differences in variability that, in reality, stem from    differences in medians.
    
    # rnd: Should the data be rounded and, if so, to which decimal? The default (-1) uses the data as is. Otherwise, rnd must be a
    #   non-negative integer. Typically, this option is not needed. However, occasionally, differences in the precision with which certain functions return values cause the merging  of two data frames to fail within the siegel.tukey  function. Only then rounding is necessary. This operation should not be performed if it affects the ranks of observations.
    
    # … arguments passed on to the Wilcoxon test. See  ?wilcox.test
    
    # Value: Among other output, the function returns the data, the Siegel-Tukey ranks, the associated Wilcoxon’s W and the p-value for a
    #   Wilcoxon test on tie-adjusted Siegel-Tukey ranks (i.e., it performs and returns a Siegel-Tukey test). If significant, the group with the smaller rank sum has greater variability.
    
    # References: Sidney Siegel and John Wilder Tukey (1960) “A nonparametric sum of ranks procedure for relative spread in unpaired
    #   samples.” Journal of the American Statistical Association. See also, David J. Sheskin (2004) ”Handbook of parametric and nonparametric statistical procedures.” 3rd edition. Chapman and Hall/CRC. Boca Raton, FL.
    
    # Notes: The Siegel-Tukey test has relatively low power and may, under certain conditions, indicate significance due to differences in
    #   medians rather than differences in variabilities (consider using the argument adjust.median).
    
    # Output (in this order)
    
    # 1. Group medians (after median adjustment if specified)
    # 2. Wilcoxon-test for between-group differences in medians (after the median  adjustment if specified)
    # 3. Data, group membership, and the Siegel-Tukey ranks
    # 4. Mean Siegel-Tukey rank by group (smaller values indicate greater variability)
    # 5. Siegel-Tukey test (Wilcoxon test on tie-adjusted  Siegel-Tukey ranks)
    
    is.formula <- function(x) class(x) == "formula"
    
    if (is.formula(x)) {
        y <- do.call(c, list(as.name(all.vars(x)[2])), envir = parent.frame(2))
        x <- do.call(c, list(as.name(all.vars(x)[1])), envir = parent.frame(2))  # I am using parent.frame(2) since if the name of the variable in the equation is 'x', then we will mistakenly get the function in here instead of the vector.
        id.col <- TRUE
        # print(x)
        # print(ls.str())
        # data=data.frame(c(x,y),rep(c(0,1),c(length(x),length(y))))
        # print(data)
    }
    
    if (id.col == FALSE) {
        data = data.frame(c(x, y), rep(c(0, 1), c(length(x), length(y))))
    } else {
        data = data.frame(x, y)
    }
    names(data) = c("x", "y")
    data = data[order(data$x), ]
    if (rnd > -1) {
        data$x = round(data$x, rnd)
    }
    
    if (adjust.median == T) {
        cat("\n", "Adjusting medians...", "\n", sep = "")
        data$x[data$y == 0] = data$x[data$y == 0] - (median(data$x[data$y == 
            0]))
        data$x[data$y == 1] = data$x[data$y == 1] - (median(data$x[data$y == 
            1]))
    }
    cat("\n", "Median of group 1 = ", median(data$x[data$y == 0]), 
        "\n", sep = "")
    cat("Median of group 2 = ", median(data$x[data$y == 1]), "\n", 
        "\n", sep = "")
    cat("Testing median differences...", "\n")
    print(wilcox.test(data$x[data$y == 0], data$x[data$y == 1]))
    
    # The following must be done for the case when id.col==F
    x <- data$x
    y <- data$y
    
    cat("Performing Siegel-Tukey rank transformation...", "\n", 
        "\n")     
    
    sort.x <- sort(data$x)
    sort.id <- data$y[order(data$x)]
    
    data.matrix <- data.frame(sort.x, sort.id)
    
    base1 <- c(1, 4)
    iterator1 <- matrix(seq(from = 1, to = length(x), by = 4)) - 
        1
    rank1 <- apply(iterator1, 1, function(x) x + base1)
    
    iterator2 <- matrix(seq(from = 2, to = length(x), by = 4))
    base2 <- c(0, 1)
    rank2 <- apply(iterator2, 1, function(x) x + base2)

    
    if (length(rank1) == length(rank2)) {
        rank <- c(rank1[1:floor(length(x)/2)], rev(rank2[1:ceiling(length(x)/2)]))
    } else {
        rank <- c(rank1[1:ceiling(length(x)/2)], rev(rank2[1:floor(length(x)/2)]))
    }
    
    unique.ranks <- tapply(rank, sort.x, mean)
    unique.x <- as.numeric(as.character(names(unique.ranks)))
    
    rank.matrix <- data.frame(unique.x, unique.ranks)
    
    ST.matrix <- merge(data.matrix, rank.matrix, by.x = "sort.x", 
        by.y = "unique.x")
    
    print(ST.matrix)
    
    cat("\n", "Performing Siegel-Tukey test...", "\n", sep = "")
    
    ranks0 <- ST.matrix$unique.ranks[ST.matrix$sort.id == 0]
    ranks1 <- ST.matrix$unique.ranks[ST.matrix$sort.id == 1]
    
    cat("\n", "Mean rank of group 0: ", mean(ranks0), "\n", sep = "")
    cat("Mean rank of group 1: ", mean(ranks1), "\n", sep = "")
    
    print(wilcox.test(ranks0, ranks1, alternative = alternative, 
        mu = mu, paired = paired, exact = exact, correct = correct, 
        conf.int = conf.int, conf.level = conf.level))
} 

# Supões que X e Y têm a mesma mediana
X=c(9,11,15)
Y=c(6,8,10,13)

teste=siegel.tukey(X,Y)
# lembrando: wilcox.text subtrai Wx do mínimo. neste caso, 3 obs para X, min(W)=6
######## MUITO IMPORTANTE: PERÇEBAM OUTPUT DA FUNÇÃO: ELE PRIMEIRO FAZ TESTE DA MEDIANA

# outra funcao
require(jmuOutlier)
siegel.test(X,Y)


# Exemplo: Tabela 6.27 Siegel
X=c(0.62,1.1,0.82,0.68,0.78,0.75,0.76,0.47)
Y=c(0.89,0.7,0.8,0.74,0.85,0.67,0.69,0.89,0.77)
siegel.tukey(X,Y)
siegel.test(X,Y)





3.4. Teste de Ansari-Bradley

ex <- data.frame(
    dados = c(X,Y),
    trat = factor(rep(c("X", "Y"), c(length(X), length(Y)))))

ansari_test(dados ~ trat, data = ex, distribution = "exact")


3.5 Teste de Posto-Similaridade de Moses
# Teste de Moses
# não supõe mesma mediana (como Siegel-Tukey)

# Programa Clécio
Moses.estat <-function(X,Y,k){
m=length(X)
n=length(Y)              
# k=nº de observações em cada subgrupo
m1=trunc(m/k)
n1=trunc(n/k) 
X1=sample(X, m1*k)
Y1=sample(Y, n1*k)
DX=rep(0,m1)
DY=rep(0,n1)
for (j in 1:m1){
aux1=X1[(k*(j-1)+1):(j*k)]
DX[j]=sum((aux1-mean(aux1))^2)} 
for (j in 1:n1){
aux1=Y1[(k*(j-1)+1):(j*k)]
DY[j]=sum((aux1-mean(aux1))^2)}
Moses=list(DY=DY,DX=DX)
return(Moses)
}

Moses.test <-function(X,Y,k,alpha=0.05,alternative='two.sided'){
m=length(X)
n=length(Y)              
# k=nº de observações em cada subgrupo
m1=trunc(m/k)
n1=trunc(n/k) 
X1=sample(X, m1*k)
Y1=sample(Y, n1*k)
DX=rep(0,m1)
DY=rep(0,n1)
for (j in 1:m1){
aux1=X1[(k*(j-1)+1):(j*k)]
DX[j]=sum((aux1-mean(aux1))^2)} 
for (j in 1:n1){
aux1=Y1[(k*(j-1)+1):(j*k)]
DY[j]=sum((aux1-mean(aux1))^2)}
aa=wilcox.test(DX,DY,alternative=alternative)
return(aa)
}

Moses.test(X,Y,4)

require(DescTools)
MosesTest(X,Y)

3.6 Teste de Postos Quadrados de Conover

SquaredRanks.estat <-function(X,Y,mu_x=NA,mu_y=NA){
# mu_x e mu_y: entre com valores conhecidos ou médias amostrais, caso contrário
if (is.na(mu_x)) (mu_x=mean(X))
if (is.na(mu_y)) (mu_y=mean(Y))
n=length(X)
m=length(Y)
nm=max(n,m)
N=m+n
U=abs(X-mu_x)
V=abs(Y-mu_y)
AmComb=c( U,V)
pac=rank(AmComb)
Rx=pac[1:n]
n1=n+1
Ry=pac[n1:N]
Tx=sum(Rx^2)
Ty=sum(Ry^2)
EX=n*(N+1)*(2*N+1)/6
VX=n*m*(N+1)*(2*N+1)*(8*N+11)/180
Txp=(Tx-EX)/sqrt(VX)
# se existem nós
R2m=1/N*(Tx+Ty)
R4=sum(Rx^4)+sum(Ry^4)
T1=(Tx-n*R2m)/sqrt(n*m/(N*(N-1))*R4-n*m/(N-1)*(R2m^2))
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
sumI=sum(is.wholenumber(pac))
if (nm<10) SR<-Tx else {
if (sumI==N) SR<-Txp else SR<-T1  }
return(SR)
}

SquaredRanks.test <-function(X,Y,mu_x=NA,mu_y=NA){     # para N assintotico
# mu_x e mu_y: entre com valores conhecidos ou médias amostrais, caso contrário
if (is.na(mu_x)) (mu_x=mean(X))
if (is.na(mu_y)) (mu_y=mean(Y))
n=length(X)
m=length(Y)
nm=max(n,m)
N=m+n
U=abs(X-mu_x)
V=abs(Y-mu_y)
AmComb=c( U,V)
pac=rank(AmComb)
Rx=pac[1:n]
n1=n+1
Ry=pac[n1:N]
Tx=sum(Rx^2)
Ty=sum(Ry^2)
EX=n*(N+1)*(2*N+1)/6
VX=n*m*(N+1)*(2*N+1)*(8*N+11)/180
Txp=(Tx-EX)/sqrt(VX)
# se existem nós
R2m=1/N*(Tx+Ty)
R4=sum(Rx^4)+sum(Ry^4)
T1=(Tx-n*R2m)/sqrt(n*m/(N*(N-1))*R4-n*m/(N-1)*(R2m^2))
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
sumI=sum(is.wholenumber(pac))
if (nm<10) SR<-Tx else {
if (sumI==N) SR<-Txp else SR<-T1}
pvalor=2*pnorm(-abs(SR))   # somente assintotico
return(c(SR,pvalor))
}


SquaredRanks.estat(X,Y)

ex <- data.frame(
    dados = c(X,Y),
    trat = factor(rep(c("X", "Y"), c(length(X), length(Y)))))

conover_test(dados ~ trat, data = ex, distribution = "exact")


########################## Fazer exemplos com todos os testes ############################
require(coin)
require(jmuOutlier)
require(DescTools)

# Ex. simulado
X=rnorm(50,0,1)
Y=rnorm(45,2,1.5)
ex <- data.frame(
    dados = c(X,Y),
    trat = factor(rep(c("X", "Y"), c(length(X), length(Y)))))

mood.test(X,Y) 
mood_test(dados ~ trat, data = ex, distribution = "exact")

klotz_test(dados ~ trat, data = ex)#, distribution = "exact")

siegel.tukey(X,Y)
siegel.test(X,Y)

ansari_test(dados ~ trat, data = ex, distribution = "exact")

Moses.test(X,Y,4) # pode variar k
MosesTest(X,Y)

conover_test(dados ~ trat, data = ex, distribution = "exact")  # usar default caso deh problema de espaço/memoria


# Ex.6.9 Siegel, Tabela 6.28
X=c(2.5,2.48,2.45,2.32,2.32,2.31,2.28,2.27,2.25,2.22,2.22,2.18,2.16,2.12,2.12,2.05,1.9)
Y=c(2.1,2.0,1.8,1.7,1.6,1.55,1.4,1.4,1.3,1.25,1.1,1.03,0.98,0.86,0.85,0.7,0.65)

# H1: var(X)<var(Y)

ex <- data.frame(
    dados = c(X,Y),
    trat = factor(rep(c("X", "Y"), c(length(X), length(Y)))))

mood.test(X,Y,alternative='less') 
mood_test(dados ~ trat, data = ex,alternative='less')

klotz_test(dados ~ trat, data = ex, alternative='less')

siegel.tukey(X,Y,alternative='greater')
siegel.test(X,Y,alternative='less')

ansari_test(dados ~ trat, data = ex, alternative='less')

Moses.test(X,Y,4,alternative='less') # pode variar k
MosesTest(X,Y,alternative='less')

conover_test(dados ~ trat, data = ex, alternative='less')  


# Fazer para exemplos abaixo                                                               

# Ex. 2 Conover, pág. 464
X=c(7.6,8.4,8.6,8.7,9.3,9.9,10.1,10.6,11.2)
Y=c(5.2,5.7,5.9,6.5,6.8,8.2,9.1,9.8,10.8,11.3,11.5,12.3,12.5,13.4,14.6)    

ex <- data.frame(
    dados = c(X,Y),
    trat = factor(rep(c("X", "Y"), c(length(X), length(Y)))))

mood.test(X,Y,alternative='less') 
mood_test(dados ~ trat, data = ex,alternative='less')

klotz_test(dados ~ trat, data = ex, alternative='less')

siegel.tukey(X,Y,alternative='greater')
siegel.test(X,Y,alternative='less')

ansari_test(dados ~ trat, data = ex, alternative='less')

Moses.test(X,Y,4,alternative='less') # pode variar k
MosesTest(X,Y,alternative='less')

conover_test(dados ~ trat, data = ex, alternative='less')  

 

# Ex.6.9 Siegel, Tabela 6.28
X=c(2.5,2.48,2.45,2.32,2.32,2.31,2.28,2.27,2.25,2.22,2.22,2.18,2.16,2.12,2.12,2.05,1.9)
Y=c(2.1,2.0,1.8,1.7,1.6,1.55,1.4,1.4,1.3,1.25,1.1,1.03,0.98,0.86,0.85,0.7,0.65)

ex <- data.frame(
    dados = c(X,Y),
    trat = factor(rep(c("X", "Y"), c(length(X), length(Y)))))

mood.test(X,Y,alternative='less') 
mood_test(dados ~ trat, data = ex,alternative='less')

klotz_test(dados ~ trat, data = ex, alternative='less')

siegel.tukey(X,Y,alternative='greater')
siegel.test(X,Y,alternative='less')

ansari_test(dados ~ trat, data = ex, alternative='less')

Moses.test(X,Y,4,alternative='less') # pode variar k
MosesTest(X,Y,alternative='less')

conover_test(dados ~ trat, data = ex, alternative='less')  



##################################################################################################################################
##################################################################################################################################
##################################################################################################################################

### Simulando poder dos testes

n1=500
n2=500        # DEMORA MUUUIIIIITO. SÓ MOSTRAR RESULTADOS                  
K=1000
pv_mood=rep(0,K)
pv_klotz=rep(0,K)
pv_siegel=rep(0,K)
pv_ansari=rep(0,K)
pv_moses=rep(0,K)
pv_postos=rep(0,K)
for (k in 1:K){

X=rnorm(n1) # rt(n1,3)    
Y=rnorm(n2) # rt(n2,3)

ex <- data.frame(
    dados = c(X,Y),
    trat = factor(rep(c("X", "Y"), c(n1, n2))))

aa=mood.test(X,Y)   # Mood
pv_mood[k]=aa$p.value

aa=klotz_test(dados ~ trat, data = ex)      # Klotz
bb=capture.output(aa)
cc=strsplit(bb[5], "p-value = ")
dd=unlist(cc)
pv_klotz[k]=as.numeric(dd[2])

aa=siegel.test(X,Y)     # Siegel-Tukey
pv_siegel[k]=aa$p.value  

aa=ansari_test(dados ~ trat, data = ex)     # Ansari-Bradley
bb=capture.output(aa)
cc=strsplit(bb[5], "p-value = ")
dd=unlist(cc)
pv_ansari[k]=as.numeric(dd[2])

aa=MosesTest(X,Y)              # Moses
pv_moses[k]=aa$p.value

aa=SquaredRanks.test(X,Y)      # Postos Quadrados
pv_postos[k]=aa[2]

}
AA = matrix(0,K,6)
AA[,1]=pv_mood
AA[,2]=pv_klotz
AA[,3]=pv_siegel
AA[,4]=pv_ansari
AA[,5]=pv_moses
AA[,6]=pv_postos
# Aproxima de alpha sob H0 e cresce se H1 fica longe de 0
mean(pv_mood<0.05)
mean(pv_klotz<0.05)
mean(pv_siegel<0.05)
mean(pv_ansari<0.05)
mean(pv_moses<0.05)
mean(pv_postos<0.05)

> mean(pv_mood<0.05)
[1] 0.047
> mean(pv_klotz<0.05)
[1] 0.042
> mean(pv_siegel<0.05)
[1] 0.052
> mean(pv_ansari<0.05)
[1] 0.05
> mean(pv_moses<0.05)
[1] 0.048
> mean(pv_postos<0.05)
[1] 0.048




###### H0 falsa, quem tem mais poder sob essa configuracao de amostras
n1=100
n2=100    # DEMORA MUUUUIIIITO. SÓ MOSTRAR OUTPUT.                   
K=1000

for (k in 1:K){

X=rnorm(n1)   
Y=rnorm(n2,0,2)   # H0 falsa
#X=rt(n2,9)
#Y=rt(n2,3) # mesma media, mas cauda pesada
# simetricas
# assimetricas
#X=rsn(n1,0,2,3)
#Y=rsn(n2,0,1,3)  #

ex <- data.frame(
    dados = c(X,Y),
    trat = factor(rep(c("X", "Y"), c(n1, n2))))

aa=mood.test(X,Y)   # Mood
pv_mood[k]=aa$p.value

aa=klotz_test(dados ~ trat, data = ex)      # Klotz
bb=capture.output(aa)
cc=strsplit(bb[5], "p-value = ")
dd=unlist(cc)
pv_klotz[k]=as.numeric(dd[2])

aa=siegel.test(X,Y)     # Siegel-Tukey
pv_siegel[k]=aa$p.value  

aa=ansari_test(dados ~ trat, data = ex)     # Ansari-Bradley
bb=capture.output(aa)
cc=strsplit(bb[5], "p-value = ")
dd=unlist(cc)
pv_ansari[k]=as.numeric(dd[2])

aa=MosesTest(X,Y)              # Moses
pv_moses[k]=aa$p.value

aa=SquaredRanks.test(X,Y)      # Postos Quadrados
pv_postos[k]=aa[2]

}
AA = matrix(0,K,6)
AA[,1]=pv_mood
AA[,2]=pv_klotz
AA[,3]=pv_siegel
AA[,4]=pv_ansari
AA[,5]=pv_moses
AA[,6]=pv_postos

AA1 = matrix(0,K,6)
for (k in 1:K){AA1[k,]=AA[k,]<=min(AA[k,])}
#AA1
colSums(AA1)

> colSums(AA1)
[1]   1 111 822   1  46  19    # melhores Siegel-Tukey, depois Klotz  =>X e Y Normais, mesma media. Testar para outras configuracoes. 

##################################################################################################################################
##################################################################################################################################
##################################################################################################################################

Pegar dados, fazer teste de aderencia. Se rejeitar, fazer testes de locacao e/ou escala

















##################################################################################################################################
##################################################################################################################################
##################################################################################################################################
# exemplos de funcoes na library coin

## Serum Iron Determination Using Hyland Control Sera
## Hollander and Wolfe (1999, p. 147, Tab 5.1)
sid <- data.frame(
    serum = c(111, 107, 100, 99, 102, 106, 109, 108, 104, 99,
              101, 96, 97, 102, 107, 113, 116, 113, 110, 98,
              107, 108, 106, 98, 105, 103, 110, 105, 104,
              100, 96, 108, 103, 104, 114, 114, 113, 108, 106, 99),
    method = gl(2, 20, labels = c("Ramsay", "Jung-Parekh"))
)

## Exact Ansari-Bradley test
pvalue(ansari_test(serum ~ method, data = sid,
                   distribution = "exact"))


## Platelet Counts of Newborn Infants
## Hollander and Wolfe (1999, p. 171, Tab. 5.4)
platelet <- data.frame(
    counts = c(120, 124, 215, 90, 67, 95, 190, 180, 135, 399,
               12, 20, 112, 32, 60, 40),
    treatment = factor(rep(c("Prednisone", "Control"), c(10, 6)))
)


