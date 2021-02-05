#Ocupamos el data frame "temporadas" guardado como archivo CSV
setwd("D:\\ACS\\BEDU\\Modulo 2\\Postwork_3")
data<-read.csv("LaLiga17-20.csv")
library(dplyr)
library(ggplot2)
data<-select(data,"Date":"FTR")
pm.el<-table(data$FTHG)/length(data$FTHG)
pm.ev<-table(data$FTAG)/length(data$FTAG)
pm.el<-as.data.frame(pm.el)
pm.ev<-as.data.frame(pm.ev)
pm.el<-rename(pm.el,Goles = Var1,ProbMarg = Freq)
pm.ev<-rename(pm.ev,Goles = Var1,ProbMarg = Freq)
install.packages("patchwork")
library(patchwork)
equipo.local<-
ggplot(pm.el,aes(x = Goles, y = ProbMarg)) + 
  geom_bar(stat = "identity", fill = "orange") + 
  labs(x = "Número de Goles", y = "Probabilidad Marginal") + 
  ggtitle("Probabilidades Marginales: Goles Equipo Local") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_text(aes(label= round(ProbMarg,3)), vjust=-0.3, size=3.5)
equipo.visitante<-
ggplot(pm.ev,aes(x = Goles, y = ProbMarg)) + 
  geom_bar(stat = "identity", fill = "green3") + 
  labs(x = "Número de Goles", y = "Probabilidad Marginal") + 
  ggtitle("Probabilidades Marginales: Goles Equipo Visitante") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_text(aes(label= round(ProbMarg,3)), vjust=-0.3, size=3.5)
equipo.local+equipo.visitante

dev.off()
pc1<-table(data$FTHG)/length(data$FTHG)
pc2<-table(data$FTAG)/length(data$FTAG)
pc<-matrix(0,length(pc1),length(pc2))
pc
for (i in 1:length(pc1)) {
  for (j in 1:length(pc2)) {
    pc[i,j] = pc1[i]*pc2[j]
  }
}
colnames(pc)<-c(0:6)
rownames(pc)<-c(0:8)
pc
library(reshape2)
pc<-melt(pc)
pc<-rename(pc,GolesLocal = Var1, GolesVisitante = Var2,Probabilidad = value)
pc
ggplot(pc,aes(x = GolesLocal , y = GolesVisitante, fill = Probabilidad),)+
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "darkolivegreen2", high = "darkolivegreen4", mid = "darkolivegreen3", 
                       midpoint = .06, limit = c(0,.12), space = "Lab") +
  theme_minimal()

