#nnet
library(nnet)
library(caret)
library(NeuralNetTools)

#wczytanie danych
SN <- read.csv("D:/Karol/SEM IX/MUM II/Lab_7/data.csv", sep=";")
SNV <- read.csv("D:/Karol/SEM IX/MUM II/Lab_7//dataValidate.csv", sep=";")
#zmiana nmiennej na zmienn typu factor
SN$Grupa.Preferencji<-factor(SN$Grupa.Preferencji)
SNV$Grupa.Preferencji<-factor(SNV$Grupa.Preferencji)

#budowa sieci
nn1 <- nnet(Grupa.Preferencji ~., data = SN, size = 50,decay = 5e-4, maxit = 200)
#wyrysowanie sieci
plotnet(nn1, struct = struct)
#wypisanie wartosci wag
neuralweights(nn1)
#podstawienie nowych danych do zbioru uczacego
result<-predict(nn1,newdata=SN, type='class')
result<-factor(as.integer(result))
#macierz pomylek dla zbioru uczacego
confusionMatrix(result,SN$Grupa.Preferencji)
result_val<-predict(nn1,newdata=SNV, type='class')
result_val<-factor(as.integer(result_val))
#macierz pomylek dla zbioru walidacyjnego
confusionMatrix(result_val,SNV$Grupa.Preferencji)