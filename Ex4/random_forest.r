library(randomForest)

SN<-read.csv("D:/Karol/SEM IX/MUM II/Lab_5_6/mieszkania.csv", header=TRUE, sep=",")

DATA<-SN

names(DATA)<-c("size", "cost", "fited", "nearCenter", "class")

# ntrees - liczba drzew.

mod <- randomForest(class~., data=DATA, ntrees=100)
a<-predict(mod, newdata=DATA)
a<-round(a,0)

library(caret)

confusionMatrix(factor(a),factor(DATA$class))

# Podstawienie danych ze zbioru walidacyjnego

SNV<-read.csv("D:/Karol/SEM IX/MUM II/Lab_5_6/mieszkania_validate.csv", header=TRUE, sep=",")

names(SNV)<-c("size", "cost", "fited", "nearCenter", "class")

b<-predict(mod, newdata=SNV)
b<-round(b,0)
confusionMatrix(factor(b),factor(SNV$class))


