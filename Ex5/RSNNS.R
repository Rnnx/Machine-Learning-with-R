#RSNNS
library(RSNNS)
library(NeuralNetTools)

#wczytanie danych
SN <- read.csv("D:/Karol/SEM IX/MUM II/Lab_7/dataAll.csv", sep=";")
names(SN)<-c("size","cost","fited", "center","class")
WE<-SN[,1:4]
WY<-decodeClassLabels(SN[,5])
#rozdzielenie zbioru na uczacy i walidacyjny
SN<-splitForTrainingAndTest(WE,WY,ratio=0.3)
#budowa modelu
model=mlp(SN$inputsTrain,SN$targetsTrain,size=c(50,50),learnFuncParams = 0.1, maxit = 200, inputsTest = SN$inputsTest, targetsTest = SN$targetsTest )
#statystyki modelu
summary(model)
#wyrysowanie sieci
plotnet(model, struct = struct)

predictions=predict(model,SN$inputsTest)
plotROC(predictions[,1], SN$targetsTest[,1])
plotIterativeError(model)
neuralweights(model)
predictions=round(predictions,0)
predictions=factor(encodeClassLabels(predictions))
a<-factor(encodeClassLabels(SN$targetsTest))
#macierz pomylek
confusionMatrix(a,predictions)
library(caret)
confusionMatrix(a,predictions)