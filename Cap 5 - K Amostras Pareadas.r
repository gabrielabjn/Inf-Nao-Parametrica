1. Estatística Q de Cochran

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
print(c("Estatística Q","p_valor"))
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


#Exemplo 1, Conover, pag. 252
X=matrix(rep(0,12*3),nrow=12,ncol=3,dimnames=list(c(),c("Esportista 1","Esportista 2","Esportista 3")))
X[1,]=c(1,1,1)
X[2,]=c(1,1,1)
X[3,]=c(0,1,0)
X[4,]=c(1,1,0)
X[5,]=c(0,0,0)
X[6,]=c(1,1,1)
X[7,]=c(1,1,1)
X[8,]=c(1,1,0)
X[9,]=c(0,0,1)
X[10,]=c(0,1,0)
X[11,]=c(1,1,1)
X[12,]=c(1,1,1)

cochran.mctest(X)

require(RVAideMemoire)
n=nrow(X)
ntrat=ncol(X)
response <- as.vector(t(X))
fact <- gl(ntrat,1,n*ntrat,labels=LETTERS[1:3])
block <- gl(n,3)
cochran.qtest(response~fact|block)


# Exemplo: Tabela 7.1 (Siegel)
X=matrix(rep(0,18*3),nrow=18,ncol=3,dimnames=list(c(),c("Entrev.1","Entrev.2","Entrev3")))
X[2,]=c(1,1,0)
X[3,]=c(0,1,0)
X[5,]=c(1,0,0)
X[6,]=c(1,1,0)
X[7,]=c(1,1,0)
X[8,]=c(0,1,0)
X[9,]=c(1,0,0)
X[11,]=c(1,1,1)
X[12,]=c(1,1,1)
X[13,]=c(1,1,0)
X[14,]=c(1,1,0)
X[15,]=c(1,1,0)
X[16,]=c(1,1,1)
X[17,]=c(1,1,0)
X[18,]=c(1,1,0)
X
cochran.mctest(X)


n=nrow(X)
ntrat=ncol(X)
response <- as.vector(t(X))
fact <- gl(ntrat,1,n*ntrat,labels=LETTERS[1:3])
block <- gl(n,3)
cochran.qtest(response~fact|block)



########################################################################################################################
########################################################################################################################

2. ANOVA
?friedman.test
?quade.test
- ANOVA: friedman.test{stats}
- Comparações múltiplas: friedman{agricolae}
- ANOVA de Quade: quade.test{stats}
- Comparações múltiplas: quade.mc (função notas de aula)

quade.mc <-function(X){
k=ncol(X)
N=nrow(X)
pX=matrix(0,N,k)
for (i in 1:N) pX[i,]=rank(X[i,])
A=rep(0,N)    
for (i in 1:N) A[i]=max(X[i,])-min(X[i,])
Q=rank(A)
S=matrix(0,N,k)
for (i in 1:N){
    for (j in 1:k) S[i,j]=Q[i]*(pX[i,j]-(k+1)/2)}
Sj=colSums(S)
A2=sum(S^2)
B=1/N*sum(Sj^2)
comb <- combn(k, 2)
nn <- ncol(comb)
estatN=rep(0,nn)
pvS=rep(0,nn)
for (r in 1:nn){
estatN[r]= abs(Sj[comb[1,r]]-Sj[comb[2,r]])/sqrt((2*N*(A2-B))/((N-1)*(k-1)))
pvS[r]=2*pt(-estatN[r],(N-1)*(k-1))
# Rejeite H0 se pvalor<alpha (mesmo do teste de quade)
}
output=matrix(0,nn,2)
output[,1]=estatN
output[,2]=pvS
#rN=rank(estatN)
#output1=output[rN,]
#colnames(output1)<-c("T1","p.value")
#rownames(output1) <- paste(comb[2,rN], comb[1,rN], sep = " - ")
colnames(output)<-c("T1","p.value")
rownames(output) <- paste(comb[2,], comb[1,], sep = " - ")
return(output)
}


# Exemplo: Siegel, Tab. 7.4
X=matrix(rep(0,18*3),nrow=18,ncol=3,dimnames=list(c(),c("RR","RU","UR")))
X[1,]=c(1,3,2)
X[2,]=c(2,3,1)
X[3,]=c(1,3,2)
X[4,]=c(1,2,3)
X[5,]=c(3,1,2)
X[6,]=c(2,3,1)
X[7,]=c(3,2,1)
X[8,]=c(1,3,2)
X[9,]=c(3,1,2)
X[10,]=c(3,1,2)
X[11,]=c(2,3,1)
X[12,]=c(2,3,1)
X[13,]=c(3,2,1)
X[14,]=c(2,3,1)
X[15,]=c(2.5,2.5,1)
X[16,]=c(3,2,1)
X[17,]=c(3,2,1)
X[18,]=c(2,3,1)
X
friedman.test(X)
quade.test(X)
# Como rejeitamos a hipótese nula, procuramos agora onde as diferenças de grupo
# são significativas (pelo menos entre dois grupos existem)
# OBSERVAÇÃO: USE O MESMO ALPHA UTILIZADO NO TESTE DE FRIEDMAN 
require(agricolae)
# Para ajustar matriz X ao formato de entrada do teste (vetores)
ind=c(rep(1:18,3))
RR=as.factor("RR")
RU=as.factor("RU")
UR=as.factor("UR")
trat=c(rep("RR",18),rep("RU",18),rep("UR",18))
Y=c(X[,1],X[,2],X[,3])
res=friedman(ind,trat,Y)
res
quade.mc(X)
# preste atenção na tabela comparativa
# "Means with the same letter are not significantly different."
# se eu quiser ver todas as diferenças
res=friedman(ind,trat,Y,g=F)
res

# Exemplo 1, Conover, p. 371
data(grass)
attach(grass)
X=matrix(0,12,4)
X[,1]=evaluation[1:12]
X[,2]=evaluation[13:24]
X[,3]=evaluation[25:36]
X[,4]=evaluation[37:48]
friedman.test(X)
quade.test(X)
# comparações múltiplas
ind=c(rep(1:12,4))
G1=as.factor("G1")
G2=as.factor("G2")
G3=as.factor("G3")
G4=as.factor("G4")
trat=c(rep("G1",12),rep("G2",12),rep("G3",12),rep("G4",12))
Y=c(X[,1],X[,2],X[,3],X[,4])
res=friedman(ind,trat,Y)
res

quade.mc(X)

## Conover (1999, p. 375f):
## Numbers of five brands of a new hand lotion sold in seven stores during one week.
# Exemplo: Loção nas lojas: mostrar como se analisa as 'letras' em comparações múltiplas   

y <- matrix(c( 5,  4,  7, 10, 12,
               1,  3,  1,  0,  2,
              16, 12, 22, 22, 35,
               5,  4,  3,  5,  4,
              10,  9,  7, 13, 10,
              19, 18, 28, 37, 58,
              10,  7,  6,  8,  7),
            nrow = 7, byrow = TRUE,
            dimnames =
            list(Store = as.character(1:7),
                 Brand = LETTERS[1:5]))
y
quade.test(y)
friedman.test(y)
# comparações múltiplas
trat=as.factor(c(rep("A",7),rep("B",7),rep("C",7),rep("D",7),rep("E",7)))
indiv=as.factor(rep(1:7,5))
Y=c(y[,1],y[,2],y[,3],y[,4],y[,5])
res=  friedman(indiv,trat,Y)
res
quade.mc(y)

res=friedman(indiv,trat,Y,g=F)
res


3. Teste de Page para Amostras Ordenadas

Page.test <- function(X){
# Matriz X: nxk, n linhas (observações) e k colunas (grupos)
# ordem das colunas de X: mesma ordem das médias em H0
n=dim(X)[1]
k=dim(X)[2]
Xp=X
for (i in 1:n) Xp[i,]=rank(X[i,])
Lx=colSums(Xp)
vec=1:k
L=sum(Lx*vec)
print("L")
print(L)
if ((n<=20&&k==3)||(n<=12&&k<=10&&k>3)) {
print("Consulte Tabela N, Siegel&Castellan")
print("N  k")
print(c(n,k))
}
else {
mu_L=n*k*(k+1)^2/4
Sigma2_L=n*(k*(k^2-1))^2/(144*(k-1))
Z=(L-mu_L)/sqrt(Sigma2_L)
p_valor=1-pnorm(Z)
return(p_valor)
} 
}

#Exemplo 7.3 Siegel, Tabela 7.5 (Ler!!!)
X=matrix(c(0.797,0.794,0.838,0.815,0.876,0.772,0.801,0.801,0.888,0.908,0.853,0.747,0.923,0.982,0.951,0.859,0.942,0.976,0.883,0.887,0.956,0.913,0.837,0.902),nrow=4,ncol=6,dimnames=list(c("A","B","C","D"),c("204","104","56","30","13","0")))
X
Page.test(X)

# Exemplo Simulado
require(mvtnorm)
X=rmvnorm(50,c(0,1,2),diag(3))
colMeans(X)
Page.test(X)


##########################################################################################################################
##########################################################################################################################
# Comparações múltiplas - Friedman
friedman<=function (judge, trt, evaluation, alpha = 0.05, group = TRUE, 
    main = NULL, console = FALSE) 
{
    name.x <- paste(deparse(substitute(judge)))
    name.y <- paste(deparse(substitute(evaluation)))
    name.t <- paste(deparse(substitute(trt)))
    name.j <- paste(deparse(substitute(judge)))
    if (is.null(main)) 
        main <- paste(name.y, "~", name.j, "+", name.t)
    datos <- data.frame(judge, trt, evaluation)
    matriz <- by(datos[, 3], datos[, 1:2], function(x) mean(x, 
        na.rm = TRUE))
    matriz <- as.data.frame(matriz[, ])
    name <- as.character(colnames(matriz))
    ntr <- length(name)
    m <- dim(matriz)
    v <- array(0, m)
    for (i in 1:m[1]) {
        v[i, ] <- rank(matriz[i, ])
    }
    vv <- as.numeric(v)
    junto <- data.frame(evaluation, trt)
    Means <- tapply.stat(junto[, 1], junto[, 2], stat = "mean")
    sds <- tapply.stat(junto[, 1], junto[, 2], stat = "sd")
    nn <- tapply.stat(junto[, 1], junto[, 2], stat = "length")
    mi <- tapply.stat(junto[, 1], junto[, 2], stat = "min")
    ma <- tapply.stat(junto[, 1], junto[, 2], stat = "max")
    nr <- unique(nn[, 2])
    s <- array(0, m[2])
    for (j in 1:m[2]) {
        s[j] <- sum(v[, j])
    }
    Means <- data.frame(Means, std = sds[, 2], r = nn[, 2], Min = mi[, 
        2], Max = ma[, 2])
    names(Means)[1:2] <- c(name.t, name.y)
    means <- Means[, c(1:2, 4)]
    rownames(Means) <- Means[, 1]
    Means <- Means[, -1]
    means[, 2] <- s
    rs <- array(0, m[2])
    rs <- s - m[1] * (m[2] + 1)/2
    T1 <- 12 * t(rs) %*% rs/(m[1] * m[2] * (m[2] + 1))
    T2 <- (m[1] - 1) * T1/(m[1] * (m[2] - 1) - T1)
    if (console) {
        cat("\nStudy:", main, "\n\n")
        cat(paste(name.t, ",", sep = ""), " Sum of the ranks\n\n")
        print(data.frame(row.names = means[, 1], means[, -1]))
        cat("\nFriedman's Test")
        cat("\n===============")
    }
    A1 <- 0
    for (i in 1:m[1]) A1 <- A1 + t(v[i, ]) %*% v[i, ]
    DFerror <- (m[1] - 1) * (m[2] - 1)
    Tprob <- qt(1 - alpha/2, DFerror)
    LSD <- as.numeric(Tprob * sqrt(2 * (m[1] * A1 - t(s) %*% 
        s)/DFerror))
    C1 <- m[1] * m[2] * (m[2] + 1)^2/4
    T1.aj <- (m[2] - 1) * (t(s) %*% s - m[1] * C1)/(A1 - C1)
    T2.aj <- (m[1] - 1) * T1.aj/(m[1] * (m[2] - 1) - T1.aj)
    p.value <- 1 - pchisq(T1.aj, m[2] - 1)
    p.noadj <- 1 - pchisq(T1, m[2] - 1)
    PF <- 1 - pf(T2.aj, ntr - 1, (ntr - 1) * (nr - 1))
    if (console) {
        cat("\nAdjusted for ties")
        cat("\nValue:", T1.aj)
        cat("\nPvalue chisq :", p.value)
        cat("\nF value :", T2.aj)
        cat("\nPvalue F:", PF)
        cat("\n\nAlpha     :", alpha)
        cat("\nt-Student :", Tprob)
    }
    if (group) {
        if (console) {
            cat("\nLSD       :", LSD)
            cat("\n\nMeans with the same letter are not significantly different.")
            cat("\nGroupTreatment and Sum of the ranks\n")
        }
        s <- as.numeric(s)
        groups <- order.stat(name, s, LSD, console = console)
        names(groups)[2] <- "Sum of ranks"
        comparison = NULL
        statistics <- data.frame(Chisq = T1.aj, p.chisq = p.value, 
            F = T2.aj, p.F = PF, LSD)
    }
    if (!group) {
        comb <- combn(ntr, 2)
        nn <- ncol(comb)
        dif <- rep(0, nn)
        pvalue <- rep(0, nn)
        LCL <- dif
        UCL <- dif
        sig <- NULL
        LSD <- rep(0, nn)
        stat <- rep("ns", nn)
        for (k in 1:nn) {
            i <- comb[1, k]
            j <- comb[2, k]
            dif[k] <- s[comb[1, k]] - s[comb[2, k]]
            sdtdif <- sqrt(2 * (m[1] * A1 - t(s) %*% s)/DFerror)
            pvalue[k] <- 2 * round(1 - pt(abs(dif[k])/sdtdif, 
                DFerror), 6)
            LSD[k] <- round(Tprob * sdtdif, 2)
            LCL[k] <- dif[k] - LSD[k]
            UCL[k] <- dif[k] + LSD[k]
            sig[k] <- " "
            if (pvalue[k] <= 0.001) 
                sig[k] <- "***"
            else if (pvalue[k] <= 0.01) 
                sig[k] <- "**"
            else if (pvalue[k] <= 0.05) 
                sig[k] <- "*"
            else if (pvalue[k] <= 0.1) 
                sig[k] <- "."
        }
        tr.i <- means[comb[1, ], 1]
        tr.j <- means[comb[2, ], 1]
        comparison <- data.frame(Difference = dif, pvalue = pvalue, 
            sig. = sig, LCL, UCL)
        rownames(comparison) <- paste(tr.i, tr.j, sep = " - ")
        if (console) {
            cat("\n\nComparison between treatments\nSum of the ranks\n\n")
            print(comparison)
        }
        statistics <- data.frame(Chisq = T1.aj, p.chisq = p.value, 
            F = T2.aj, p.F = PF)
        groups = NULL
    }
    parameters <- data.frame(Df = ntr - 1, ntr = ntr, t.value = Tprob)
    rownames(parameters) <- " "
    rownames(statistics) <- " "
    output <- list(statistics = statistics, parameters = parameters, 
        means = Means, comparison = comparison, groups = groups)
    invisible(output)
}
