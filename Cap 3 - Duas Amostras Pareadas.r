
1) Teste do Sinal

# vari?veis pelo menos ORDINAIS
# pares (Xi,Yi)
# Entre com X e Y ou Diferen?a

D0<-c(0,1,2,-3,4)

D0=X-Y
T0=length(D0[D0>0])
T0=sum(D0>0)
n=length(D0)
n_=length(D0[D0!=0])  # tam atualizado de n, desconsiderando os "nos"
binom.test(T0,n_,p=0.5,alternative = "two.sided")  # alternative = c("two.sided", "less", "greater"),

# Exemplo, Siegel, Tabela 5.4
T0=11
n=14
binom.test(T0,n,p=0.5,alternative ="greater")
binom.test(T0,n,p=0.5,alternative ="less") # a titulo de comparacao

#######################################################################################################################
#######################################################################################################################

2) Teste de McNemar
                
mcnemar.test(tabela)  # funcao pacote stats R

# Caso os dados estejam em uma matriz nx2
tabij=matrix(0,2,2)# tabela McNemar
s1=apply(X,1,sum)
tabij[1,1]=sum(s1==0)
tabij[2,2]=sum(s1==2)
s2=X[,1]-X[,2]
tabij[2,1]=sum(s2==1)
tabij[1,2]=sum(s2==-1)

# Estatistica e teste de Siegel
mcnemar.stest<-function(X){
if (nrow(X)>2){# se entrar com matriz nx2
    tabij=matrix(0,2,2)# tabela McNemar
    s1=apply(X,1,sum)
    tabij[1,1]=sum(s1==0)
    tabij[2,2]=sum(s1==2)
    s2=X[,1]-X[,2]
    tabij[2,1]=sum(s2==1)
    tabij[1,2]=sum(s2==-1) 
    }
else {tabij=X}
n=sum(diag(tab))
if (n <= 20){
T0=tab[1,1]
binom.test(T0,n,p=0.5)
}
else {
X2=(abs(tabij[1,1]-tabij[2,2]-1))^2/sum(diag(tabij))
pv=1-pchisq(X2,1)
print(c("Estat?stica Q","p.valor"))
print(c(X2,pv))
}
  #return(c(X2,pv))
}

# Conover diz que se A+D <=20, usar T2=A ~ Binomial(A+D,0.5). Como este teste eh exato, posso usar somente Binomial

# Exemplo Siegel, Tabela 5.3
tab=matrix(c(28,13,7,27),2,2)
mcnemar.test(tab) # eh o contrario do teste binomial

tab=matrix(c(13,27,28,7),2,2)
n=sum(diag(tab))
T0=tab[1,1]
binom.test(T0,n,p=0.5)   # pvalor proximo da minha funcao


# testar pela qui-quadrado

# estranho a funcao do R trabalha com os pares concordantes
# para trabalhar com funcao do R
tab1=matrix(c(28,7,13,27),2,2)
tab1
mcnemar.test(tab1)

# Exemplo, pag. 168
tab=matrix(c(21,12,63,4),2,2)
tab

mcnemar.stest(tab)

n=sum(diag(tab))
T0=tab[1,1]
binom.test(T0,n,p=0.5)   # pvalor proximo da minha funcao

# para trabalhar com funcao do R
tab1=matrix(c(63,4,21,12),2,2)
tab1
mcnemar.test(tab1)

#######################################################################################################################
#######################################################################################################################

3) Teste do Sinal de Wilcoxon
# vari?veis INTERVALARES
# no R: 
?wilcox.test

# Exemplo simulado
n=10
X=rnorm(n)   # fazer para diferentes tamanhos de amostra: 10, 40, 100
Y=rnorm(n)
D=X-Y
D=D[abs(D)>0]
R=sign(D)*rank(abs(D))
hist(D)
Rm=R[R>0]
Tmais=sum(Rm)
R
Rm
Tmais

wilcox.test(D)
# bate D
# P valores 'exatos'
p_esq=pnorm((sum(R)+1)/sqrt(sum(R^2)))
p_dir=1-pnorm((sum(R)-1)/sqrt(sum(R^2)))
p_bil=2*min(c(p_esq,p_dir))
p_bil    # Mesmo para pequenas amostras, o pvalor eh bem proximo do de Conover

# Exemplo 1 Conover - pag. 355
# X: escore de agressividade do g?meo que nasceu primeiro
# Y: escore de agressividade do g?meo que nasceu por ?ltimo
# Hip?tese alternativa: agressividade de quem nasce primeiro ? maior
X=c(86,71,77,68,91,72,77,91,70,71,88,87) # nasceu primeiro
Y=c(88,77,76,64,96,72,65,90,65,80,81,72)  # nasceu por ultimo

D=Y-X

# teste simetria (requisito pro WIlcoxon)
library(lawstat)
symmetry.test(D)

#H0: MId = 0
#H1: MId > 0

wilcox.test(D,altern="less") 
# Percebam que o R (wilcox.test) sempre solta a estat?stica V igual a Tmais;
# p_valor usa equa??es 7 e 8, p?g. 354 se n <=50 e n?o existem n?s


### Exemplo 2 - conover
D=c(-6.2,-4,-3.1,-2.6,-2,0.3,0.7,1.2,1.3,2.8,3.2,3.9,4.3,4.9,5.0,5.9,6.1,6.4,6.6,7.2,7.3,7.9,8.2,9.6,10.6,11.1,12.3,12.8,14,15.8)

wilcox.test(D,altern="gr")


##########################################################################################################################
##########################################################################################################################

4) Teste da Aleatoriza??o/Permuta??o
# Ir no Help HTML do R e abrir os pacotes abaixo. Veja que em exactRankTests existe rperm, dperm, pperm, qperm (e uma distrib. de probabilidade)

permut<-function(d){# d>1 vetor de diferencas; package gtools
require(gtools)
t_est=sum(d)
d=sort(abs(d))
n=length(d)
k=2^n
tvetor=c(sum(d),-sum(d))# minimo e maximo
for (i in 1:(n-1)){
nrep=choose(n,i)
aux=combinations(n,i, v=1:n, set=TRUE, repeats.allowed=FALSE)
for (j in 1:nrep){
 dj=d
 indj=aux[j,]
 dj[indj]=-d[indj]
 tj=sum(dj)
 tvetor=c(tvetor,tj)
}
}
return(sort(tvetor))
}


d=c(+19,+27,-1,+6,+7,+13,-4,+3)
aa=permut(d)
plot(aa,abs(aa))
hist(aa)

require(exactRankTests)
require(coin)

?perm.test

################################################################################
# comparando com outros testes

rmtr<-function(n,mu,Sigma,nu){
  p=length(mu)
  y<-matrix(0,n,p)
  for(i in 1:n){
    u=rgamma(1,shape=nu/2,scale=2/nu) #fator de escala da t  
    y[i,]=rmvnorm(1,mean=mu,sigma=Sigma)/sqrt(u)
  } 
  return(y) 
}



n=100                          
K=1000
pv_perm=rep(0,K)
pv_wilcox=rep(0,K)
pv_sign=rep(0,K)
sigma1=matrix(c(1,0.5,0.5,2),ncol=2)

library(coin)
library(exactRankTests)

for (k in 1:K){
#XY=rmvnorm(n,mean=c(0,0),sigma=sigma1) # Fazer sob H0 e sob H1 , fazer mu1-mu2 variando de 0 a valores mais distantes. Ver curva de ecdf(pv_xxxx)
XY<-rmtr(n,c(0,0),diag(2),3)
#
X=XY[,1]     
Y=XY[,2] 
aa=perm.test(X,Y,paired=TRUE)# 
pv_perm[k]=aa$p.value
# teste do sinal
D=X-Y
T=length(D[D>0])
aa=binom.test(T,n,1/2) 
pv_sign[k]=aa$p.value
# teste de wilcoxon
aa=wilcox.test(D)
pv_wilcox[k]=aa$p.value
}


AA = matrix(0,K,3)
AA[,1]=pv_perm
AA[,2]=pv_wilcox
AA[,3]=pv_sign

plot(ecdf(pv_perm))
abline(0,1,col='red')

AA1 = matrix(0,K,3)
for (k in 1:K){AA1[k,]=AA[k,]<=min(AA[k,])}
#AA1
colSums(AA1)

# incluindo teste T pareado
n=100                          
K=1000
pv_perm=rep(0,K)
pv_wilcox=rep(0,K)
pv_sign=rep(0,K)
pv_t=rep(0,K)
for (k in 1:K){
#XY=rmvnorm(n,mean=c(0,2),sigma=diag(2))   # Fazer sob H0 e sob H1
XY<-rmtr(n,c(0,0),diag(2),3)
#
X=XY[,1]     
Y=XY[,2] 
aa=perm.test(X,Y,paired=TRUE)# 
pv_perm[k]=aa$p.value
# teste do sinal
D=X-Y
T=length(D[D>0])
aa=binom.test(T,n,1/2) 
pv_sign[k]=aa$p.value
# teste de wilcoxon
aa=wilcox.test(D)
pv_wilcox[k]=aa$p.value
# Teste t parametrico
est_t=mean(D)/sqrt(var(D)/n)
pv_t[k]=2*pnorm(-abs(est_t))
}
AA = matrix(0,K,4)
AA[,1]=pv_perm
AA[,2]=pv_wilcox
AA[,3]=pv_sign
AA[,4]=pv_t

AA1 = matrix(0,K,4)
for (k in 1:K){AA1[k,]=AA[k,]<=min(AA[k,])}
#AA1
colSums(AA1)


######### p-valores



####
n=50  # aumente n (10, 50, 100)                        
K=1000
pv_perm=rep(0,K)
pv_wilcox=rep(0,K)
pv_sign=rep(0,K)
pv_t=rep(0,K)
###   fazer teste t pareado
for (k in 1:K){
#XY=rmvnorm(n,mean=c(0,2),sigma=diag(2))   # Fazer sob H0 e sob H1
XY<-rmtr(n,c(0,1),diag(2),3)               # Fazer sob H0 e sob H1
#
X=XY[,1]     
Y=XY[,2] 
aa=perm.test(X,Y,paired=TRUE)# 
pv_perm[k]=aa$p.value
# teste do sinal
D=X-Y
T=length(D[D>0])
aa=binom.test(T,n,1/2) 
pv_sign[k]=aa$p.value
# teste de wilcoxon
aa=wilcox.test(D)
pv_wilcox[k]=aa$p.value
# Teste t parametrico
est_t=mean(D)/sqrt(var(D)/n)
pv_t[k]=2*pnorm(-abs(est_t))

}
# Aproxima de alpha sob H0 e cresce se H1 fica longe de 0
alpha=0.5
mean(pv_perm<alpha)
mean(pv_wilcox<alpha)
mean(pv_sign<alpha)
mean(pv_t<alpha)


# p-valor do wilcoxon eh assintotico 
# estatistica eh exata (usar ela ate n = 50)

# variabilidade favorece h0