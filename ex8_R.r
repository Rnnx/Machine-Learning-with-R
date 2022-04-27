library("kohonen")
library('corrplot')

creditData<-read.csv("http://jolej.linuxpl.info/CC_GENERAL.csv", header=TRUE)
balance <- creditData$BALANCE
balance_freq <- creditData$BALANCE_FREQUENCY
purchases <- creditData$PURCHASES
oneof_purchases <- creditData$ONEOFF_PURCHASES
installments <- creditData$INSTALLMENTS_PURCHASES
cashAdvance <- creditData$CASH_ADVANCE
purchase_freq <- creditData$PURCHASES_FREQUENCY
oneof_pur_freq <- creditData$ONEOFF_PURCHASES_FREQUENCY
pur_inst_freq <- creditData$PURCHASES_INSTALLMENTS_FREQUENCY
cash_adv_freq <- creditData$CASH_ADVANCE_FREQUENCY
cash_adv_trx <- creditData$CASH_ADVANCE_TRX
purch_trx <- creditData$PURCHASES_TRX
credit_limit <- creditData$CREDIT_LIMIT
payment <- creditData$PAYMENTS
mpay <- creditData$MINIMUM_PAYMENTS
price_full_paym <- creditData$PRC_FULL_PAYMENT
tenure <- creditData$TENURE

data1<-data.frame(balance,balance_freq,purchases,oneof_purchases,installments,cashAdvance,
                  purchase_freq,oneof_pur_freq,pur_inst_freq,cash_adv_freq,cash_adv_trx,
                  purch_trx,payment,price_full_paym,tenure)
data2<-cor(data1)
corrplot(data2, method="number")
corrplot(data2, method="ellipse")
corrplot(data2, method="color")
corrplot(data2, method="pie")
corrplot(data2, method="circle")

Dane.ist <- c(balance,purchases,payment)

Dane.sc = scale(Dane.ist)

Dane.grid = somgrid(xdim = 20, ydim=20, topo="rectangular")

Dane.som = som(Dane.sc, grid=Dane.grid, rlen=300, alpha=c(0.05,0.01))

summary(Dane.som)

plot(Dane.som, type="changes")
plot(Dane.som, type="count")
plot(Dane.som, type="mapping")
plot(Dane.som, type="dist.neighbours")
plot(Dane.som, type="codes")

groups = 15

Dane.hc = cutree(hclust(dist(Dane.som$codes[[1]])), groups)

plot(Dane.som, type="codes", bgcol=rainbow(groups)[Dane.hc])

add.cluster.boundaries(Dane.som, Dane.hc)
