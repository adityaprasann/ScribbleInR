# Title     : TODO
# Objective : TODO
# Created by: adityaprasann
# Created on: 8/9/18
library(readxl)
library(tidyverse)
library(ggplot2)
library(kdensity)


FinData <- read_excel("./WQU_Econometrics_Module1_Data.xlsx", col_types = c ("date","numeric","numeric","numeric","numeric",
"numeric","numeric","numeric","numeric"))
hist(FinData$APPL_lr,breaks=64,freq=FALSE,main="APPL",xlab='apple daily log returns')
kde_AAPL <- kdensity(FinData$APPL_lr,start="normal")
lines(kde_AAPL, col="blue")
lines(kde_AAPL, plot_start=TRUE, col="red")
legend("topright",c("kernel estimate","normal"), lty=c(1,1), lwd=c(1,1), col=c("blue","red"))

mean_apple = mean(FinData$APPL_lr)
sd_apple = sd(FinData$APPL_lr)
min_apple = min(FinData$APPL_lr)
max_apple = max(FinData$APPL_lr)

APPL_std = (FinData$APPL_lr - mean_apple)/sd_apple

qqnorm(APPL_std, main="Normal Q-Q Plot Apple", plot.it=TRUE, datax=TRUE)
qqline(APPL_std, datax=FALSE, distribution=qnorm, probs=c(0.25,0.75), qtype=7)

hist(FinData$IBM_lr,breaks=64,freq=FALSE,main="IBM",xlab='IBM daily log returns')
kde_IBM <- kdensity(FinData$IBM_lr,start="normal")
lines(kde_IBM, col="blue")
lines(kde_IBM, plot_start=TRUE, col="red")
legend("topright",c("kernel estimate","normal"), lty=c(1,1), lwd=c(1,1), col=c("blue","red"))

mean_ibm = mean(FinData$IBM_lr)
sd_ibm = sd(FinData$IBM_lr)
min_ibm = min(FinData$IBM_lr)
max_ibm = max(FinData$IBM_lr)

IBM_std = (FinData$IBM_lr - mean_ibm)/sd_ibm

qqnorm(IBM_std, main="Normal Q-Q Plot ibm", plot.it=TRUE, datax=TRUE)
qqline(IBM_std, datax=FALSE, distribution=qnorm, probs=c(0.25,0.75), qtype=7)

cov_AI = cov(APPL_std, IBM_std)
print(cov_AI)

cor_AI = cor(APPL_std, IBM_std)
print(cor_AI)







