
#Importa los datos de soccer de las temporadas 2017/2018, 2018/2019 y 2019/2020 de la primera división de la liga española a R

setwd("~/Desktop/BEDU/R/Postwork/España/datos")
datos <- lapply(dir(), read.csv) 

#selecciona únicamente las columnas Date, HomeTeam, AwayTeam, FTHG, FTAG y FTR
library(dplyr)
datos <- lapply(datos, select, Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)

head(datos[[1]])
str(datos[[2]])
head(datos[[3]])

#Hacer un solo dataframe 
Futbol_España <- do.call(rbind, datos)

#Corregir fechas
str(Futbol_España)
Futbol_España <- mutate(Futbol_España, Date=(as.Date(Date,"%d/%m/%Y")))

#Separar las columnas de goles en casa y goles de visita
Goles_casa <- Futbol_España$FTHG
Goles_visitante <- Futbol_España$FTAG

#Tablas de frecuencia relativa
Freq_goles_casa <- table(Goles_casa)

Freq_goles_visita <- table(Goles_visitante)

#probabilidad (marginal) de que el equipo que juega en casa anote x goles 
Prob_goles_casa <- prop.table(Freq_goles_casa)

#probabilidad (marginal) de que el equipo que juega como visitante anote y goles 
Prob_goles_visita <- prop.table(Freq_goles_visita)

#probabilidad (conjunta) de que el equipo que juega en casa anote x goles 
#y el equipo que juega como visitante anote y goles

Freq_conjunta <- table(Goles_casa, Goles_visitante)
Prob_conjunta <- prop.table(Freq_conjunta)


#### PW 4
#Obtén una tabla de cocientes al dividir estas probabilidades conjuntas por el producto de las probabilidades marginales correspondientes.

df.Prob_goles_casa <- as.data.frame(Prob_goles_casa)
df.Prob_conjunta<- as.data.frame(Prob_conjunta)
df.Prob_goles_visita <- as.data.frame(Prob_goles_visita)


#Prob_goles_casa*Prob_goles_visita

r1 <- (df.Prob_goles_casa[1,2]* df.Prob_goles_visita[,2])
r2 <- (df.Prob_goles_casa[2,2]* df.Prob_goles_visita[,2])
r3 <- (df.Prob_goles_casa[3,2]* df.Prob_goles_visita[,2])
r4 <- (df.Prob_goles_casa[4,2]* df.Prob_goles_visita[,2])
r5 <- (df.Prob_goles_casa[5,2]* df.Prob_goles_visita[,2])
r6 <- (df.Prob_goles_casa[6,2]* df.Prob_goles_visita[,2])
r7 <- (df.Prob_goles_casa[7,2]* df.Prob_goles_visita[,2])
r8 <- (df.Prob_goles_casa[8,2]* df.Prob_goles_visita[,2])
r9 <- (df.Prob_goles_casa[9,2]* df.Prob_goles_visita[,2])

#Data frame de la multiplicacion de los marginales 
mult <- rbind(r1,r2,r3,r4,r5,r6,r7,r8,r9)

#tabla de cocientes

cocientes <- Prob_conjunta/mult

#bootstrap 

a <- numeric(1000)

#calcular el error estandar, de muestras aleatorias con n 30, de los cocientes originales
#This serves as an estimate of the standard error of αˆ estimated from the original data set.
for (i in 1:1000) {
  sample<- sample(cocientes, size= 30, replace=T)
  a[i] <- sd(sample)/sqrt(length(sample))
}

#graficar histograma
hist(a)

