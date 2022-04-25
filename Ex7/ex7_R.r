library(readxl)
library(RSNNS)
library(quantmod)

#packageSet <- c("car", "abind", "aplpack", "colorspace", "effects", "Hmisc",
#                "leaps", "zoo", "lmtest", "mvtnorm", "multcomp", "relimp", "rgl", "RODBC",
#                "clv", "rpart.plot", "flexclust", "e1071", "sem", "Rcmdr","foreign","tree","rpart","rattle","ipred","randomForest","dplyr","sqldf","genalg","corrplot","caret","nnet","RSNNS","NeuralNetTools","devtools","kohonen")
#install.packages(packageSet)
#rm(packageSet)

googleData<-read.csv("http://jolej.linuxpl.info/GOOG.csv", header=TRUE)

#ekstrakcja zmiennej objasnianej
close<-scale(as.numeric(googleData$close))

train<-1:1000
# tworzenie okna czasowego
y<-as.zoo(close)
x1<-Lag(y,k=1)
x2<-Lag(y,k=2)

close_p<-cbind(y,x1,x2)
close_p<-close_p[-(1:2),]
inputs<-close_p[,2:3]
outputs<-close_p[,1]

fit<-elman(inputs[train],
           outputs[train],
           size=c(10,3),
           learnFuncParams=c(0.1),
           maxit=4500)
plotIterativeError(fit)

par(mfrow=c(1, 1))

par(mfrow=c(2, 1))
out_r<-as.vector(outputs[-train]) * attr(close, 'scaled:scale') + attr(close, 'scaled:center')
plot(out_r,type="l")

pred<-predict(fit,inputs[-train]) * attr(close, 'scaled:scale') + attr(close, 'scaled:center')
plot(pred,col="red",type="l")
wynik<-data.frame(out_r,pred)

#obliczam blad bezwzgledny 
sum(abs((out_r-pred)/out_r))/length(out_r)