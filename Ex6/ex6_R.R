library(readxl)
library(corrplot)
library(AMORE)

setwd("D:/OneDrive - Wojskowa Akademia Techniczna/WAT_SEM_9/Metody uczenia maszynowego/Lab 5")
my_data <- read_excel("2007.xlsx", sheet="Sheet1")

a<-data.frame(select(my_data, -c('data')))
b<-cor(a)
corrplot(b, method="number")

my_data <- select(my_data, c('1 USD', '1 HKD', '1 EUR', '1 CHF', '1 CYP', '1 GBP', '1 DKK', '1 EEK', '1 MTL', '1 LTL', '1 LVL', '1 RUB', '1 XDR'))

WY <- as.vector(my_data[2:201,1])
WE <- as.matrix(my_data[1:200,-1])
WY_WAL <- as.vector(my_data[202:253,1])
WE_WAL <- as.matrix(my_data[200:251,-1])

net <- newff(n.neurons=c(12,10,10,1), learning.rate.global=1e-2, momentum.global=0.5, error.criterium="TAO", Stao=NA, hidden.layer="tansig",output.layer="purelin", method="ADAPTgdwm")
#net <- newff(n.neurons=c(12,10,10,1), learning.rate.global=1e-2, momentum.global=0.5, error.criterium="TAO", Stao=NA, hidden.layer="sigmoid",output.layer="purelin", method="ADAPTgdwm")
#net <- newff(n.neurons=c(12,10,10,1), learning.rate.global=1e-2, momentum.global=0.5, error.criterium="TAO", Stao=NA, hidden.layer="tansig",output.layer="purelin", method="ADAPTgd")
#net <- newff(n.neurons=c(12,10,10,1), learning.rate.global=1e-2, momentum.global=0.5, error.criterium="TAO", Stao=NA, hidden.layer="sigmoid",output.layer="purelin", method="ADAPTgd")
#net <- newff(n.neurons=c(12,10,10,1), learning.rate.global=1e-2, momentum.global=0.5, error.criterium="TAO", Stao=NA, hidden.layer="tansig",output.layer="purelin", method="BATCHgdwm")
#net <- newff(n.neurons=c(12,10,10,1), learning.rate.global=1e-2, momentum.global=0.5, error.criterium="TAO", Stao=NA, hidden.layer="tansig",output.layer="purelin", method="BATCHgd")

result <- AMORE::train(net, WE, WY,WE_WAL,WY_WAL, error.criterium="LMS", report=TRUE, show.step=10, n.shows=30)
#podstawienie danych
WYSN <- sim(result$net,WE_WAL)
error.LMS(data.frame(round(WYSN, 4),WY_WAL))
#Wykres funkcji bledu
matplot(result$Merror, pch=21:23, bg=c("white",  "black"), type="o",col="black")