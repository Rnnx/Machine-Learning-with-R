library('BCA')
library('car')
library('RcmdrMisc')
library('sandwich')
library('relimp')
library('corrplot')

bikesDay<-read.csv("D:/Karol/SEM IX/MUM II/Lab_3/day.csv", header=TRUE)
  bikesHour<-read.csv("D:/Karol/SEM IX/MUM II/Lab_3/hour.csv", header=TRUE)

# Sprawdzenie wszystkich zmiennych
variable.summary(bikesDay)
  variable.summary(bikesHour)

# Wykres punktowy
scatterplot(cnt~factor(dteday), reg.line=lm, smooth=TRUE, spread=FALSE, id.method='mahal', id.n = 2, boxplots='xy', span=0.5, data=bikesDay)
  scatterplot(cnt~weathersit, reg.line=lm, smooth=TRUE, spread=FALSE, id.method='mahal', id.n = 2, boxplots='xy', span=0.5, data=bikesHour)

# Wykres liniowy
with(bikesDay, lineplot(mnth, cnt))
  with(bikesHour, lineplot(weathersit, cnt))

# Analizowanie zaleznosci dla wszystkich zmiennych
scatterplotMatrix(~cnt+season+yr+mnth+holiday+weekday+workingday+weathersit+temp+atemp+hum+windspeed+casual+registered, 
                  reg.line=lm, smooth=TRUE, spread=FALSE, span=0.5, id.n=0, diagonal = 'boxplot', data=bikesDay)
  scatterplotMatrix(~cnt+season+yr+mnth+hr+holiday+weekday+workingday+weathersit+temp+atemp+hum+windspeed+casual+registered, 
                    reg.line=lm, smooth=TRUE, spread=FALSE, span=0.5, id.n=0, diagonal = 'boxplot', data=bikesHour)

# Analiza korelacji zmiennych
a<-data.frame(bikesDay$season,bikesDay$yr,bikesDay$mnth,bikesDay$holiday,
              bikesDay$weekday,bikesDay$workingday,bikesDay$weathersit,bikesDay$temp,
              bikesDay$atemp,bikesDay$hum,bikesDay$windspeed,bikesDay$casual,bikesDay$registered,bikesDay$cnt)
c<-cor(a)
corrplot(c)

  b<-data.frame(bikesHour$season,bikesHour$yr,bikesHour$mnth,bikesHour$hr,
                bikesHour$holiday,bikesHour$weekday,bikesHour$workingday,bikesHour$weathersit,
                bikesHour$temp,bikesHour$atemp,bikesHour$hum,bikesHour$windspeed,bikesHour$casual,bikesHour$registered,bikesHour$cnt)
  d<-cor(b)
  corrplot(d)

# Model regresji liniowej
bikesDay.model1 <- lm(cnt~season+yr+mnth+holiday+weekday+workingday+weathersit+temp+atemp+hum+windspeed, data=bikesDay)
summary(bikesDay.model1)
# Model po redukcji nieistotnych zmiennych
bikesDay.model2 <- lm(cnt~season+yr+mnth+holiday+weekday+workingday+weathersit+atemp+hum+windspeed, data=bikesDay)
summary(bikesDay.model2)

  bikesHour.model1 <- lm(cnt~season+yr+mnth+hr+holiday+weekday+workingday+weathersit+temp+atemp+hum+windspeed, data=bikesHour)
  summary(bikesHour.model1)
  # Model po redukcji nieistotnych zmiennych
  bikesHour.model2 <- lm(cnt~season+yr+hr+holiday+weekday+weathersit+temp+atemp+hum+windspeed, data=bikesHour)
  summary(bikesHour.model2)

# Model nieliniowy
# Logarytmowanie zmiennych ciaglych
bikesDay$log.cnt <- with(bikesDay, log(cnt))
bikesDay$log.atemp <- with(bikesDay, log(atemp))
bikesDay$log.hum <- with(bikesDay, log(hum+1e-64))
bikesDay$log.windspeed <- with(bikesDay, log(windspeed))
  
# Budowa modelu nieliniowego
bikesDay.nonLinear.model1 <- lm(log.cnt ~ season+yr+mnth+holiday+weekday+workingday+weathersit+log.atemp+log.hum+log.windspeed, data=bikesDay)
summary(bikesDay.nonLinear.model1)
  
bikesDay.nonLinear.model2 <- lm(log.cnt ~ season+yr+mnth+holiday+weekday+workingday+weathersit+log.atemp+log.hum+log.windspeed, data=bikesDay)
summary(bikesDay.nonLinear.model2)

  # Logarytmowanie zmiennych ciaglych
  bikesHour$log.cnt <- with(bikesHour, log(cnt))
  bikesHour$log.temp <- with(bikesHour, log(temp+1e-64))
  bikesHour$log.atemp <- with(bikesHour, log(atemp+1e-64))
  bikesHour$log.hum <- with(bikesHour, log(hum+1e-64))
  bikesHour$log.windspeed <- with(bikesHour, log(windspeed+1e-64))
  
  # Budowa modelu nieliniowego
  bikesHour.nonLinear.model1 <- lm(log.cnt ~ season+yr+hr+holiday+weekday+weathersit+log.temp+log.atemp+log.hum+log.windspeed, data=bikesHour)
  summary(bikesHour.nonLinear.model1)