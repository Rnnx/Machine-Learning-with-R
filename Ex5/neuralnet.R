#Neutralnet
library(neuralnet)
library(caret)
library(RSNNS)

#wczytanie danych
SN <- read.csv("D:/Karol/SEM IX/MUM II/Lab_7/data.csv", sep=";")
SNV <- read.csv("D:/Karol/SEM IX/MUM II/Lab_7//dataValidate.csv", sep=";")
names(SN)<-c("size","cost","fited","center","class")
names(SNV)<-c("size","cost","fited","center","class")
SN$class<-factor(SN$class)
SNV$class<-factor(SNV$class)
#budowa modelu
nnet <- neuralnet(class ~ . , data=SN, hidden=c(20,10), linear.output=FALSE)
#macierz wyniku
nnet$result.matrix
#wyrysowanie wykresu sieci
plot(nnet)
predicted.output=predict(nnet,SNV)
predicted.output=round(predicted.output,0)
print(predicted.output)
predicted.output=encodeClassLabels(predicted.output)
predicted.output=factor(predicted.output)
#budowa macierzy pomylek
confusionMatrix(SNV$class,predicted.output)
