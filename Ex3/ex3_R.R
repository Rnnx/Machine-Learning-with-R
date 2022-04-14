library(BCA)
library(relimp)
library(car)
library(RcmdrMisc)
library(sqldf)

# Load dataset
bankData<-read.csv2("D:/Karol/SEM IX/MUM II/Lab_4/bank.csv", header=TRUE)

bankData$Sample <- create.samples(bankData, est = 0.7, val = 0.3)

# Konwersja na factor
bankData$loan <- as.factor(bankData$loan)
bankData$housing <- as.factor(bankData$housing)
bankData$default <- as.factor(bankData$default)

bankData <- within(bankData, {
  Y.Bin <- Recode(y, '"yes"=1; "no"=0', as.factor=FALSE)
})

with(bankData, plotMeans(Y.Bin, loan, error.bars="none"))
with(bankData, plotMeans(Y.Bin, job, error.bars="none"))
with(bankData, plotMeans(Y.Bin, default, error.bars="none"))
with(bankData, plotMeans(Y.Bin, poutcome, error.bars="none"))


# Model liniowy
GLM.2 <- glm(Y.Bin ~ age + job + marital + education + default + 
               balance + housing + loan + contact + day + month + 
               duration + campaign + pdays + previous + poutcome, 
             family=binomial(logit), data=bankData)

summary(GLM.2)
Anova(GLM.2)
1 - (GLM.2$deviance/GLM.2$null.deviance) # McFadden R2

# Redukcja
GLM.2 <- glm(Y.Bin ~ contact + month + duration + poutcome, 
             family=binomial(logit), data=bankData)
summary(GLM.2)
Anova(GLM.2)
1 - (GLM.2$deviance/GLM.2$null.deviance) # McFadden R2


# Ujemne check
min <- data.frame(c(sqldf('SELECT min(balance) FROM bankData WHERE balance<0')))

names(min)<-c("Balance")

for(i in 1:length(bankData$balance)){
  if(bankData$balance[i] <= 0)
    bankData$balance[i] = 1;
}

bankData$balance.log <- with(bankData, log(balance))
bankData$campaign.log <- with(bankData, log(campaign))
bankData$duration.log <- with(bankData, log(duration))

# Budowa modelu nieliniowego
LogCCS <- glm(Y.Bin ~ age + job + marital + education + default + 
                balance.log + housing + loan + contact + day + month + 
                duration.log + campaign.log + pdays + previous + poutcome, family=binomial(logit), 
              data=bankData, subset=Sample=="Estimation")
summary(LogCCS)

Anova(LogCCS)
1 - (LogCCS$deviance/LogCCS$null.deviance) # McFadden R2


# Ponowne sprawdzenie + redukcja
LogCCS <- glm(Y.Bin ~ contact + month + duration.log + poutcome, family=binomial(logit), 
              data=bankData, subset=Sample=="Estimation")
summary(LogCCS)
Anova(LogCCS)
1 - (LogCCS$deviance/LogCCS$null.deviance) # McFadden R2


# Budowa modelu mieszanego
MixedCCS <- glm(Y.Bin ~ campaign + duration + balance + age + job + marital + education + default + 
                  balance.log + housing + loan + contact + day + month + 
                  duration.log + campaign.log + pdays + previous + poutcome, family=binomial(logit), data=bankData, 
                subset=Sample=="Estimation")
summary(MixedCCS)
Anova(MixedCCS)
1 - (MixedCCS$deviance/MixedCCS$null.deviance) # McFadden R2

# Redukcja modelu miesznaego
MixedCCS <- glm(Y.Bin ~contact + month + 
                  duration.log + poutcome, family=binomial(logit), data=bankData, 
                subset=Sample=="Estimation")
summary(MixedCCS)
Anova(MixedCCS)
1 - (MixedCCS$deviance/MixedCCS$null.deviance) # McFadden R2


