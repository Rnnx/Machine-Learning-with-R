library(e1071)

SN<-read.csv("D:/Karol/SEM IX/MUM II/Lab_5_6/mieszkania.csv", header=TRUE, sep=",")
SNV<-read.csv("D:/Karol/SEM IX/MUM II/Lab_5_6/mieszkania_validate.csv", header=TRUE, sep=",")

DATA<-SN

names(DATA)<-c("size", "cost", "fited", "nearCenter", "class")

names(SNV)<-c("size", "cost", "fited", "nearCenter", "class")


mod <- svm(class~., data=DATA)
a<-predict(mod, newdata=DATA)
a<-round(a,0)

library(caret)


confusionMatrix(factor(a),factor(DATA$class))

# Dla zbioru walidacyjnego

b<-predict(mod, newdata=SNV)
b<-round(b,0)
confusionMatrix(factor(b),factor(SNV$class))




     


