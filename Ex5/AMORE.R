#AMORE
library(AMORE)
library(caret)

#Wczytanie zbioru danych i zbioru walidacyjnego
SN <- read.csv("D:/Karol/SEM IX/MUM II/Lab_7/data.csv", sep=";")
SNV <- read.csv("D:/Karol/SEM IX/MUM II/Lab_7//dataValidate.csv", sep=";")

#Okreslenie typu zmiennych, budowa macierzy z danych wejsciowycj i wektorów z danych wyjsciowych
SN[,1]<-as.numeric(SN[,1])
SN[,2]<-as.numeric(SN[,2])
SN[,3]<-as.numeric(SN[,3])
SN[,4]<-as.numeric(SN[,4])
WE=as.matrix(SN[,1:4])
WY=as.vector(as.numeric(SN[,5]))
SNV[,1]<-as.numeric(SNV[,1])
SNV[,2]<-as.numeric(SNV[,2])
SNV[,3]<-as.numeric(SNV[,3])
SNV[,3]<-as.numeric(SNV[,4])
VAL_WE=as.matrix(SNV[,1:4])
VAL_WY=as.vector(as.numeric(SNV[,5]))

#budowa sieci
net <- newff(n.neurons=c(4,10,10,1), learning.rate.global=1e-2, momentum.global=0.5, error.criterium="TAO", Stao=NA, hidden.layer="tansig",output.layer="purelin", method="ADAPTgdwm")
#net <- newff(n.neurons=c(4,10,10,1), learning.rate.global=1e-2, momentum.global=0.5, error.criterium="TAO", Stao=NA, hidden.layer="sigmoid",output.layer="purelin", method="ADAPTgdwm")
#net <- newff(n.neurons=c(4,10,10,1), learning.rate.global=1e-2, momentum.global=0.5, error.criterium="TAO", Stao=NA, hidden.layer="purelin",output.layer="purelin", method="ADAPTgd")
#net <- newff(n.neurons=c(4,10,10,1), learning.rate.global=1e-2, momentum.global=0.5, error.criterium="TAO", Stao=NA, hidden.layer="tansig",output.layer="purelin", method="ADAPTgd")
#net <- newff(n.neurons=c(4,10,10,1), learning.rate.global=1e-2, momentum.global=0.5, error.criterium="TAO", Stao=NA, hidden.layer="sigmoid",output.layer="purelin", method="ADAPTgd")
#net <- newff(n.neurons=c(4,10,10,1), learning.rate.global=1e-2, momentum.global=0.5, error.criterium="TAO", Stao=NA, hidden.layer="purelin",output.layer="purelin", method="BATCHgdwm")
#net <- newff(n.neurons=c(4,10,10,1), learning.rate.global=1e-2, momentum.global=0.5, error.criterium="TAO", Stao=NA, hidden.layer="tansig",output.layer="purelin", method="BATCHgdwm")
#net <- newff(n.neurons=c(4,10,10,1), learning.rate.global=1e-2, momentum.global=0.5, error.criterium="TAO", Stao=NA, hidden.layer="purelin",output.layer="purelin", method="BATCHgd")
#net <- newff(n.neurons=c(4,10,10,1), learning.rate.global=1e-2, momentum.global=0.5, error.criterium="TAO", Stao=NA, hidden.layer="tansig",output.layer="purelin", method="BATCHgd")

result <- AMORE::train(net, WE, WY,VAL_WE,VAL_WY, error.criterium="LMS", report=TRUE, show.step=100, n.shows=20 )

#uruchomienie predykcji i zaokrglenie wyników
WYSN<-round(sim(result$net,VAL_WE),0)

wynik<- data.frame(VAL_WY,WYSN)
#wykres bledu sredniego r^2 dla danych uczacych i walidacyjnych
matplot(result$Merror, pch=21:23, bg=c("white",  "black"), type="o",col="black", xlim=c(1, 30), ylim=c(0,0.5))
WYSN<-factor(WYSN)
VAL_WY<-factor(VAL_WY)
#wyrysowanie macierzy pomylek
caret::confusionMatrix(WYSN,VAL_WY)