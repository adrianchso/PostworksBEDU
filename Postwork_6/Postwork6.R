setwd("D:\\ACS\\BEDU\\Modulo 2\\Postwork_6")
library(lubridate)
library(dplyr)
match.data<-read.csv("match.data.csv")
head(match.data);tail(match.data)
match.data<-mutate(match.data,sumagoles = home.score+away.score)
match.data<-mutate(match.data,date = as.Date(date))
str(match.data)
match.data.ts<- match.data %>% group_by(Year = year(date), Month=month(date)) %>%
  summarise(GolesPromedioMensual = mean(sumagoles))
match.data.ts<-ts(match.data.ts, start = c(2010,8), 
                  end = c(2019,12), frequency = 12)
plot(match.data.ts, xlab = "Años", ylab =  "Goles Promedio", 
     main = "Promedio Mensual de Goles Liga Española", sub = "Temporadas 2010-2019")
plot(decompose(match.data.ts))
     