# Title     : TODO
# Objective : TODO
# Created by: adityaprasann
# Created on: 22/9/18
library(tidyverse)
library(ggplot2)
library(reshape2)
library(kdensity)
library(stats)
library(tseries)
library(forecast)
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
print(logReturnsDateDf)
dates <- c('2017-09-21','2017-09-22','2017-09-25','2017-09-26','2017-09-27','2017-09-28','2017-09-29','2017-10-02','2017-10-03','2017-10-04','2017-10-05','2017-10-06','2017-10-09','2017-10-10','2017-10-11','2017-10-12','2017-10-13','2017-10-16','2017-10-17','2017-10-18','2017-10-19','2017-10-20','2017-10-23','2017-10-24','2017-10-25','2017-10-26','2017-10-27','2017-10-30','2017-10-31','2017-11-01','2017-11-02','2017-11-03','2017-11-06','2017-11-07','2017-11-08','2017-11-09','2017-11-10','2017-11-13','2017-11-14','2017-11-15','2017-11-16','2017-11-17','2017-11-20','2017-11-21','2017-11-22','2017-11-24','2017-11-27','2017-11-28','2017-11-29','2017-11-30','2017-12-01','2017-12-04','2017-12-05','2017-12-06','2017-12-07','2017-12-08','2017-12-11','2017-12-12','2017-12-13','2017-12-14','2017-12-15','2017-12-18','2017-12-19','2017-12-20','2017-12-21','2017-12-22','2017-12-26','2017-12-27','2017-12-28','2017-12-29','2018-01-02','2018-01-03','2018-01-04','2018-01-05','2018-01-08','2018-01-09','2018-01-10','2018-01-11','2018-01-12','2018-01-16','2018-01-17','2018-01-18','2018-01-19','2018-01-22','2018-01-23','2018-01-24','2018-01-25','2018-01-26','2018-01-29','2018-01-30','2018-01-31','2018-02-01','2018-02-02','2018-02-05','2018-02-06','2018-02-07','2018-02-08','2018-02-09','2018-02-12','2018-02-13','2018-02-14','2018-02-15','2018-02-16','2018-02-20','2018-02-21','2018-02-22','2018-02-23','2018-02-26','2018-02-27','2018-02-28','2018-03-01','2018-03-02','2018-03-05','2018-03-06','2018-03-07','2018-03-08','2018-03-09','2018-03-12','2018-03-13','2018-03-14','2018-03-15','2018-03-16','2018-03-19','2018-03-20','2018-03-21','2018-03-22','2018-03-23','2018-03-26','2018-03-27','2018-03-28','2018-03-29','2018-04-02','2018-04-03','2018-04-04','2018-04-05','2018-04-06','2018-04-09','2018-04-10','2018-04-11','2018-04-12','2018-04-13','2018-04-16','2018-04-17','2018-04-18','2018-04-19','2018-04-20','2018-04-23','2018-04-24','2018-04-25','2018-04-26','2018-04-27','2018-04-30','2018-05-01','2018-05-02','2018-05-03','2018-05-04','2018-05-07','2018-05-08','2018-05-09','2018-05-10','2018-05-11','2018-05-14','2018-05-15','2018-05-16','2018-05-17','2018-05-18','2018-05-21','2018-05-22','2018-05-23','2018-05-24','2018-05-25','2018-05-29','2018-05-30','2018-05-31','2018-06-01','2018-06-04','2018-06-05','2018-06-06','2018-06-07','2018-06-08','2018-06-11','2018-06-12','2018-06-13','2018-06-14','2018-06-15','2018-06-18','2018-06-19','2018-06-20','2018-06-21','2018-06-22','2018-06-25','2018-06-26','2018-06-27','2018-06-28','2018-06-29','2018-07-02','2018-07-03','2018-07-05','2018-07-06','2018-07-09','2018-07-10','2018-07-11','2018-07-12','2018-07-13','2018-07-16','2018-07-17','2018-07-18','2018-07-19','2018-07-20','2018-07-23','2018-07-24','2018-07-25','2018-07-26','2018-07-27','2018-07-30','2018-07-31','2018-08-01','2018-08-02','2018-08-03','2018-08-06','2018-08-07','2018-08-08','2018-08-09','2018-08-10','2018-08-13','2018-08-14','2018-08-15','2018-08-16','2018-08-17','2018-08-20','2018-08-21','2018-08-22','2018-08-23','2018-08-24','2018-08-27','2018-08-28','2018-08-29','2018-08-30','2018-08-31','2018-09-04','2018-09-05','2018-09-06','2018-09-07','2018-09-10','2018-09-11','2018-09-12','2018-09-13','2018-09-14','2018-09-17','2018-09-18','2018-09-19','2018-09-20')
assignDataDateDf <-cbind(dates,assignData)
print(assignDataDateDf)

assignDataDateDf1 <- melt(assignDataDateDf,id="dates")
ggplot(assignDataDateDf1,aes(x=dates,y=value,colour=variable,group=variable)) + geom_line()
ggsave("EconometricsGroupAssign1StockReturns.png")

logReturnsDateDf1 <- melt(logReturnsDateDf,id="dates")
ggplot(logReturnsDateDf1,aes(x=dates,y=value,colour=variable,group=variable)) + geom_line()
ggsave("EconometricsGroupAssign1LogReturns.png")

ggplot(data = logReturnsDateDf1, aes(x = dates, y = value, color = variable)) + geom_point()
ggsave("EconometricsGroupAssign1ScatterPlots.png")

print("The corelaatin matrix is")
print(cor(logReturnsDf))

hist(logReturnsDateDf$JPM,breaks=64,freq=FALSE,main="JPM",xlab='JPM daily log returns')
kde_JPM <- kdensity(logReturnsDateDf$JPM,start="normal")
lines(kde_JPM, col="blue")
lines(kde_JPM, plot_start=TRUE, col="red")
legend("topright",c("kernel estimate","normal"), lty=c(1,1), lwd=c(1,1), col=c("blue","red"))
mean_JPM = mean(logReturnsDateDf$JPM)
sd_JPM = sd(logReturnsDateDf$JPM)
JPM_std = (logReturnsDateDf$JPM - mean_JPM)/sd_JPM
qqnorm(JPM_std, main="Normal Q-Q Plot JPM", plot.it=TRUE, datax=TRUE)
qqline(JPM_std, datax=FALSE, distribution=qnorm, probs=c(0.25,0.75), qtype=7)

hist(logReturnsDateDf$Citibank,breaks=64,freq=FALSE,main="Citibank",xlab='Citibank daily log returns')
kde_Citibank <- kdensity(logReturnsDateDf$JPM,start="normal")
lines(kde_Citibank, col="blue")
lines(kde_Citibank, plot_start=TRUE, col="red")
legend("topright",c("kernel estimate","normal"), lty=c(1,1), lwd=c(1,1), col=c("blue","red"))
mean_Citibank = mean(logReturnsDateDf$Citibank)
sd_Citibank = sd(logReturnsDateDf$Citibank)
Citibank_std = (logReturnsDateDf$Citibank - mean_Citibank)/sd_Citibank
qqnorm(Citibank_std, main="Normal Q-Q Plot JPM", plot.it=TRUE, datax=TRUE)
qqline(Citibank_std, datax=FALSE, distribution=qnorm, probs=c(0.25,0.75), qtype=7)


hist(logReturnsDateDf$MCO,breaks=64,freq=FALSE,main="MCO",xlab='MCO daily log returns')
kde_MCO <- kdensity(logReturnsDateDf$JPM,start="normal")
lines(kde_MCO, col="blue")
lines(kde_MCO, plot_start=TRUE, col="red")
legend("topright",c("kernel estimate","normal"), lty=c(1,1), lwd=c(1,1), col=c("blue","red"))
mean_MCO = mean(logReturnsDateDf$MCO)
sd_MCO = sd(logReturnsDateDf$MCO)
MCO_std = (logReturnsDateDf$MCO - mean_MCO)/sd_MCO
qqnorm(MCO_std, main="Normal Q-Q Plot JPM", plot.it=TRUE, datax=TRUE)
qqline(MCO_std, datax=FALSE, distribution=qnorm, probs=c(0.25,0.75), qtype=7)

hist(logReturnsDateDf$Google,breaks=64,freq=FALSE,main="Google",xlab='Google daily log returns')
kde_Google <- kdensity(logReturnsDateDf$JPM,start="normal")
lines(kde_Google, col="blue")
lines(kde_Google, plot_start=TRUE, col="red")
legend("topright",c("kernel estimate","normal"), lty=c(1,1), lwd=c(1,1), col=c("blue","red"))
mean_Google = mean(logReturnsDateDf$Google)
sd_Google = sd(logReturnsDateDf$Google)
Google_std = (logReturnsDateDf$Google - mean_Google)/sd_Google
qqnorm(Google_std, main="Normal Q-Q Plot Google", plot.it=TRUE, datax=TRUE)
qqline(Google_std, datax=FALSE, distribution=qnorm, probs=c(0.25,0.75), qtype=7)

hist(logReturnsDateDf$Netflix,breaks=64,freq=FALSE,main="Netflix",xlab='Netflix daily log returns')
kde_Netflix <- kdensity(logReturnsDateDf$JPM,start="normal")
lines(kde_Netflix, col="blue")
lines(kde_Netflix, plot_start=TRUE, col="red")
legend("topright",c("kernel estimate","normal"), lty=c(1,1), lwd=c(1,1), col=c("blue","red"))
mean_Netflix = mean(logReturnsDateDf$Netflix)
sd_Netflix = sd(logReturnsDateDf$Netflix)
Netflix_std = (logReturnsDateDf$Netflix - mean_Netflix)/sd_Netflix
qqnorm(Netflix_std, main="Normal Q-Q Plot Netflix", plot.it=TRUE, datax=TRUE)
qqline(Netflix_std, datax=FALSE, distribution=qnorm, probs=c(0.25,0.75), qtype=7)

print("ARIMA model for JPM")

acf_ts_JPM <- ts(logReturnsDf$JPM)
ggAcf(acf_ts_JPM, plot = TRUE, lag.MAX = 25)
ggsave("EconometricsGroupAssignACFJPM.png")

ggPacf(acf_ts_JPM, plot = TRUE, lag.MAX = 25)
ggsave("EconometricsGroupAssignPACFJPM.png")

print(adf.test(acf_ts_JPM))

print(kpss.test(acf_ts_JPM))

jpmModel <- arima(acf_ts_JPM, method="ML")
print(Box.test(jpmModel$residuals))
summary(jpmModel)

print("ARIMA model for Citibank")

acf_ts_Citibank <- ts(logReturnsDf$Citibank)
ggAcf(acf_ts_Citibank, plot = TRUE, lag.MAX = 25)
ggsave("EconometricsGroupAssignACFCitibank.png")

ggPacf(acf_ts_Citibank, plot = TRUE, lag.MAX = 25)
ggsave("EconometricsGroupAssignPACFCitibank.png")

print(adf.test(acf_ts_Citibank))

print(kpss.test(acf_ts_Citibank))

citibankModel <- arima(acf_ts_Citibank, method="ML")
print(Box.test(citibankModel$residuals))
summary(citibankModel)

print("ARIMA model for MCO")

acf_ts_MCO <- ts(logReturnsDf$MCO)
ggAcf(acf_ts_MCO, plot = TRUE, lag.MAX = 25)
ggsave("EconometricsGroupAssignACFMCO.png")

ggPacf(acf_ts_MCO, plot = TRUE, lag.MAX = 25)
ggsave("EconometricsGroupAssignPACFMCO.png")

print(adf.test(acf_ts_MCO))

print(kpss.test(acf_ts_MCO))

mcoModel <- arima(acf_ts_MCO, method="ML")
print(Box.test(mcoModel$residuals))
summary(mcoModel)

print("ARIMA model for Google")

acf_ts_Google <- ts(logReturnsDf$Google)
ggAcf(acf_ts_Google, plot = TRUE, lag.MAX = 25)
ggsave("EconometricsGroupAssignACFGoogle.png")

ggPacf(acf_ts_Google, plot = TRUE, lag.MAX = 25)
ggsave("EconometricsGroupAssignPACFGoogle.png")

print(adf.test(acf_ts_Google))

print(kpss.test(acf_ts_Google))

googleModel <- arima(acf_ts_Google, method="ML")
print(Box.test(googleModel$residuals))
summary(googleModel)

print("ARIMA model for Netflix")

acf_ts_Netflix <- ts(logReturnsDf$Netflix)
ggAcf(acf_ts_Netflix, plot = TRUE, lag.MAX = 25)
ggsave("EconometricsGroupAssignACFNetflix.png")

ggPacf(acf_ts_Netflix, plot = TRUE, lag.MAX = 25)
ggsave("EconometricsGroupAssignPACFNetflix.png")

print(adf.test(acf_ts_Netflix))

print(kpss.test(acf_ts_Netflix))

netflixModel <- arima(acf_ts_Netflix, method="ML")
print(Box.test(netflixModel$residuals))
summary(netflixModel)




