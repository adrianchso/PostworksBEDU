setwd('D:/ACS/BEDU/Modulo 2')
la.liga<-read.csv('SP1.csv')
la.liga
x<-la.liga$FTHG
x
y<-la.liga$FTAG
goles<-data.frame(local = x,visitante = y)
contl<-table(goles$local)
totall<-sum(contl)
z<-rep(totall,7)
freql<-contl/z
freql
contv<- table(goles$visitante)
c<-c(0)
names(c)<-c(6)
c
contvc<-c(contv,c)
contvc
z
freqv<-contvc/z
freqv
tabla<-rbind(freqv,freql)
row.names(tabla)<-c("Frec. Rel. Goles Local","Frec. Rel. Goles Visitante")
conj<-matrix(0,7,7,T)
for (i in 1:7) {
  for (j in 1:7) {
    conj[i,j]=freql[i]*freqv[j]
  }
}
conj
matriz_conj<-matrix(conj,7,7,T)
colnames(matriz_conj)<-c(0:6)
rownames(matriz_conj)<-c(0:6)
matriz_conj
