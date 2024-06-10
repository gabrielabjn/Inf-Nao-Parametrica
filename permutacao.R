permut<-function(d){# d>1 vetor de diferencas; package gtools
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
