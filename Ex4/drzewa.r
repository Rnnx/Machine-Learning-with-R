library(tree)
library(caret)
library(rattle)
library(rpart)

#Zadanie A
SN <- read.csv("http://jolej.linuxpl.info/SN.csv", sep=";")
DATA<-SN

drzewo.DATA <-  tree(Klasa~.,data=DATA)
sd <- summary(drzewo.DATA)

plot(drzewo.DATA)
text(drzewo.DATA)

a<-predict(drzewo.DATA, newdata=DATA)
a<-round(a,0)

confusionMatrix(factor(a),factor(DATA$Klasa))

RPART.1 <- rpart(Klasa ~. , data=DATA, cp=0.0015)

plotcp(RPART.1)
fancyRpartPlot(RPART.1)

a<-predict(RPART.1, newdata=DATA)
a<-round(a,0)

confusionMatrix(factor(a),factor(DATA$Klasa))

#Zadanie B
mieszkania<-read.csv("D:/Karol/SEM IX/MUM II/Lab_5_6/mieszkania.csv", header=TRUE, sep=",")

names(mieszkania)<-c("D", "T", "U", "BC", "Klasa")

drzewo.mieszkania <- tree(Klasa~.,data=mieszkania)

sd <- summary(drzewo.mieszkania)

plot(drzewo.mieszkania)
text(drzewo.mieszkania)

a<-predict(drzewo.mieszkania, newdata=mieszkania)
a<-round(a,0)

confusionMatrix(factor(a),factor(mieszkania$Klasa))

drzewo2.mieszkania <- rpart(Klasa ~. , data=mieszkania, cp=0.0026)
plotcp(drzewo2.mieszkania)

fancyRpartPlot(drzewo2.mieszkania)

a<-predict(drzewo2.mieszkania, newdata=mieszkania)
a<-round(a,0)

confusionMatrix(factor(a),factor(mieszkania$Klasa))