# Title     : TODO
# Objective : TODO
# Created by: adityaprasann
# Created on: 4/10/18
library(ggplot2)
library(fGarch)
library(tidyverse)
library(reshape2)
library(stats)
library(tseries)
library(vars)
library(timeSeries)
library(tsDyn)
# Read data from CSV file
assignFilesData <- read.csv("./EconometricsGroupAssign.csv")
# Load data to a data frame
assignData = data.frame(assignFilesData$JPM.Adjusted,assignFilesData$C.Adjusted,assignFilesData$MCO.Adjusted,assignFilesData$GOOGL.Adjusted,assignFilesData$NFLX.Adjusted)
symbols <- list('JPM','Citibank','MCO','Google','Netflix')
count <- 1
logReturnsDf <- data.frame(diff(log(assignData[,count]), lag=1),
diff(log(assignData[,count+1]), lag=1),
diff(log(assignData[,count+2]), lag=1),
diff(log(assignData[,count+3]), lag=1),
diff(log(assignData[,count+4]), lag=1))

names(logReturnsDf) <- c(symbols[[count]][1], symbols[[count+1]][1], symbols[[count+2]][1], symbols[[count+3]][1], symbols[[count+4]][1])

dates <- c('2017-09-21','2017-09-22','2017-09-25','2017-09-26','2017-09-27','2017-09-28','2017-09-29','2017-10-02','2017-10-03','2017-10-04','2017-10-05','2017-10-06','2017-10-09','2017-10-10','2017-10-11','2017-10-12','2017-10-13','2017-10-16','2017-10-17','2017-10-18','2017-10-19','2017-10-20','2017-10-23','2017-10-24','2017-10-25','2017-10-26','2017-10-27','2017-10-30','2017-10-31','2017-11-01','2017-11-02','2017-11-03','2017-11-06','2017-11-07','2017-11-08','2017-11-09','2017-11-10','2017-11-13','2017-11-14','2017-11-15','2017-11-16','2017-11-17','2017-11-20','2017-11-21','2017-11-22','2017-11-24','2017-11-27','2017-11-28','2017-11-29','2017-11-30','2017-12-01','2017-12-04','2017-12-05','2017-12-06','2017-12-07','2017-12-08','2017-12-11','2017-12-12','2017-12-13','2017-12-14','2017-12-15','2017-12-18','2017-12-19','2017-12-20','2017-12-21','2017-12-22','2017-12-26','2017-12-27','2017-12-28','2017-12-29','2018-01-02','2018-01-03','2018-01-04','2018-01-05','2018-01-08','2018-01-09','2018-01-10','2018-01-11','2018-01-12','2018-01-16','2018-01-17','2018-01-18','2018-01-19','2018-01-22','2018-01-23','2018-01-24','2018-01-25','2018-01-26','2018-01-29','2018-01-30','2018-01-31','2018-02-01','2018-02-02','2018-02-05','2018-02-06','2018-02-07','2018-02-08','2018-02-09','2018-02-12','2018-02-13','2018-02-14','2018-02-15','2018-02-16','2018-02-20','2018-02-21','2018-02-22','2018-02-23','2018-02-26','2018-02-27','2018-02-28','2018-03-01','2018-03-02','2018-03-05','2018-03-06','2018-03-07','2018-03-08','2018-03-09','2018-03-12','2018-03-13','2018-03-14','2018-03-15','2018-03-16','2018-03-19','2018-03-20','2018-03-21','2018-03-22','2018-03-23','2018-03-26','2018-03-27','2018-03-28','2018-03-29','2018-04-02','2018-04-03','2018-04-04','2018-04-05','2018-04-06','2018-04-09','2018-04-10','2018-04-11','2018-04-12','2018-04-13','2018-04-16','2018-04-17','2018-04-18','2018-04-19','2018-04-20','2018-04-23','2018-04-24','2018-04-25','2018-04-26','2018-04-27','2018-04-30','2018-05-01','2018-05-02','2018-05-03','2018-05-04','2018-05-07','2018-05-08','2018-05-09','2018-05-10','2018-05-11','2018-05-14','2018-05-15','2018-05-16','2018-05-17','2018-05-18','2018-05-21','2018-05-22','2018-05-23','2018-05-24','2018-05-25','2018-05-29','2018-05-30','2018-05-31','2018-06-01','2018-06-04','2018-06-05','2018-06-06','2018-06-07','2018-06-08','2018-06-11','2018-06-12','2018-06-13','2018-06-14','2018-06-15','2018-06-18','2018-06-19','2018-06-20','2018-06-21','2018-06-22','2018-06-25','2018-06-26','2018-06-27','2018-06-28','2018-06-29','2018-07-02','2018-07-03','2018-07-05','2018-07-06','2018-07-09','2018-07-10','2018-07-11','2018-07-12','2018-07-13','2018-07-16','2018-07-17','2018-07-18','2018-07-19','2018-07-20','2018-07-23','2018-07-24','2018-07-25','2018-07-26','2018-07-27','2018-07-30','2018-07-31','2018-08-01','2018-08-02','2018-08-03','2018-08-06','2018-08-07','2018-08-08','2018-08-09','2018-08-10','2018-08-13','2018-08-14','2018-08-15','2018-08-16','2018-08-17','2018-08-20','2018-08-21','2018-08-22','2018-08-23','2018-08-24','2018-08-27','2018-08-28','2018-08-29','2018-08-30','2018-08-31','2018-09-04','2018-09-05','2018-09-06','2018-09-07','2018-09-10','2018-09-11','2018-09-12','2018-09-13','2018-09-14','2018-09-17','2018-09-18','2018-09-19')
logReturnsDateDf <- cbind(dates,logReturnsDf)

print("<--- GARCH Calculations --->")

print("### GARCH Summary JPM ###")
model.JPM <- garchFit(formula = ~arma(3,2) + garch(2,1), data = logReturnsDateDf$JPM, trace=F)
summary(model.JPM)

print("### GARCH Summary Citibank ###")
model.Citibank <- garchFit(formula = ~arma(3,2) + garch(2,1), data = logReturnsDateDf$Citibank, trace=F)
summary(model.Citibank)

print("### GARCH Summary MCO ###")
model.MCO <- garchFit(formula = ~arma(3,2) + garch(2,1), data = logReturnsDateDf$MCO, trace=F)
summary(model.MCO)

print("### GARCH Summary Google ###")
model.Google <- garchFit(formula = ~arma(3,2) + garch(2,1), data = logReturnsDateDf$Google, trace=F)
summary(model.Google)

print("### GARCH Summary Netflix ###")
model.Netflix <- garchFit(formula = ~arma(3,2) + garch(2,1), data = logReturnsDateDf$Netflix, trace=F)
summary(model.Netflix)


assignSDDf <-data.frame(sqrt(252) * model.JPM@sigma.t, sqrt(252) * model.Citibank@sigma.t, sqrt(252) * model.MCO@sigma.t,
sqrt(252) * model.Google@sigma.t,sqrt(252) * model.Netflix@sigma.t)
names(assignSDDf) <- c(symbols[[count]][1], symbols[[count+1]][1], symbols[[count+2]][1], symbols[[count+3]][1], symbols[[count+4]][1])

index <- sequence(251)
assignSDIdxDf <- cbind(index, assignSDDf)

assignSDIdxDf1 <- melt(assignSDIdxDf,id="index")
ggplot(assignSDIdxDf1,aes(x=index,y=value,colour=variable,group=variable)) + geom_line()
ggsave("EconometricsGroupAssign2Garch.png")
print("<--- GARCH Calculations --->")

print("<--- TGARCH Calculations --->")
print("### TGARCH Summary JPM ###")
model.TGARCH.JPM <- garchFit(~aparch(1,1), data = logReturnsDateDf$JPM, delta = 2, include.delta = FALSE)
summary(model.TGARCH.JPM)

print("### TGARCH Summary Citibank ###")
model.TGARCH.Citibank <- garchFit(~aparch(1,1), data = logReturnsDateDf$JPM, delta = 2, include.delta = FALSE)
summary(model.TGARCH.Citibank)

print("### TGARCH Summary MCO ###")
model.TGARCH.MCO <- garchFit(~aparch(1,1), data = logReturnsDateDf$JPM, delta = 2, include.delta = FALSE)
summary(model.TGARCH.MCO)

print("### TGARCH Summary Google ###")
model.TGARCH.Google <- garchFit(~aparch(1,1), data = logReturnsDateDf$JPM, delta = 2, include.delta = FALSE)
summary(model.TGARCH.Google)

print("### TGARCH Summary Netflix ###")
model.TGARCH.Netflix <- garchFit(~aparch(1,1), data = logReturnsDateDf$JPM, delta = 2, include.delta = FALSE)
summary(model.TGARCH.Netflix)


assignSDDf.TGARCH <-data.frame(sqrt(252) * model.TGARCH.JPM@sigma.t, sqrt(252) * model.TGARCH.Citibank@sigma.t, sqrt(252) * model.TGARCH.MCO@sigma.t,
sqrt(252) * model.TGARCH.Google@sigma.t,sqrt(252) * model.TGARCH.Netflix@sigma.t)
names(assignSDDf.TGARCH) <- c(symbols[[count]][1], symbols[[count+1]][1], symbols[[count+2]][1], symbols[[count+3]][1], symbols[[count+4]][1])

assignSDIdxDf.TGARCH <- cbind(index, assignSDDf.TGARCH)

assignSDIdxDf1.TGARCH <- melt(assignSDIdxDf.TGARCH,id="index")
ggplot(assignSDIdxDf1.TGARCH,aes(x=index,y=value,colour=variable,group=variable)) + geom_line()
ggsave("EconometricsGroupAssign2TGARCH.png")
print("<--- TGARCH Calculations --->")

print("<--- IRF Calculations --->")
cor(logReturnsDf)
VAR_model <- VAR(logReturnsDf, lag.max=-12, type="none", ic="AIC")
print(summary(VAR_model))

VAR_irf <- irf(VAR_model, n.ahead=13, boot=TRUE, ci=0.95)
plot(VAR_irf)

print("<--- IRF Calculations --->")

print("<--- Co Integration Tests --->")
jotest_eigen <- ca.jo(logReturnsDf, type="eigen", K=9, ecdet="none", spec="longrun")
print(summary(jotest_eigen))
jotest_trace <- ca.jo(logReturnsDf, type="trace", K=9, ecdet="none", spec="longrun")
print(summary(jotest_trace))

print("<--- Co Integration Tests --->")


print("<--- VECM Tests --->")

VECM_fit = VECM(logReturnsDf,1, r=1, include="const",estim="ML", LRinclude="none")
print(VECM_fit)

print("<--- VECM Tests --->")


