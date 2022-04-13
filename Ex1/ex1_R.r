library(genalg)

allServicesNum <- c(148,87,234,176,23,845,234,111)
currentServicesNum <- c(148,87,234,176,23,845,234,111)

servicePrice <- c(50,237,128,34,27,239,78,99)

zMatrix <- matrix(c(2,0,1,7,0,3,1,0,
                    0,2,0,2,4,6,0,9,
                    0,1,1,0,0,10,0,0,
                    4,0,0,5,1,0,8,0,
                    8,3,3,0,0,0,0,1,
                    1,4,0,0,11,0,1,4,
                    0,5,1,9,0,0,3,0), nrow = 7, byrow = TRUE)

evaluate <- function(X=c()) {
  
returnVal = NA;

xTmp <- c(0,0,0,0,0,0,0,0)
v1 <- c(0,0,0,0,0,0,0,0)

for(i in 1:nrow(zMatrix)) {
  for(j in 1:ncol(zMatrix)) {
    if(X[i] > 0) {
      xTmp[j] <- (X[i]*zMatrix[i,j])
    }
    currentServicesNum <- currentServicesNum - xTmp
    xTmp <- c(0,0,0,0,0,0,0,0)
  }
}
xTmp <- c(0,0,0,0,0,0,0,0)

#v1 <- allServicesNum - currentServicesNum


for (i in 1:length(currentServicesNum)) {
  if(currentServicesNum[i] < 0) {
    returnVal = 1000000000000000
    break
  } else {
    v1[i] <- allServicesNum[i] - currentServicesNum[i]
  }
}

if(v1[1]&v1[2]&v1[3]&v1[4]&v1[5]&v1[6]&v1[7]&v1[8] >= 0){
  returnVal = sum(c(-50,-237,-128,-34,-27,-239,-78,-99)*v1);
} else {
  returnVal = 1000000000000000
}

returnVal
}

rbga.results = rbga(c(0,0,0,0,0,0,0),c(25,5,84,23,18,2,17),popSize=1000, evalFunc=evaluate, 
                    iters=2000, verbose=TRUE, mutationChance=0.01, elitism = 0)

summary(rbga.results,echo=TRUE)

best <- rbga.results$population[rbga.results$evaluations == min(rbga.results$best),]
#best <- best[1,]

print(best)

plot(rbga.results)
