# Title     : TODO
# Objective : TODO
# Created by: adityaprasann
# Created on: 22/9/18
library(readxl)
library(tidyverse)
library(ggplot2)
library(stats)
library(tseries)
library(forecast)

gdpData <- read_excel("./WQU_Econometrics_Module3_Data_RealUSDP.xls", col_names = T, col_types = c ("date","numeric","numeric"))

ggplot(data = gdpData, mapping = aes(x=gdpData$DATE, y=gdpData$RGDP)) + geom_line(color = "dark blue") + labs(x="", y="US Inflation Data")

ggsave("EcoInflation.png")

acf_ts <- ts(gdpData$RGDP)
ggAcf(acf_ts, plot = TRUE, lag.MAX = 25)
ggsave("EcoInflationACF.png")

ggPacf(acf_ts, plot = TRUE, lag.MAX = 25)
ggsave("EcoInflationPACF.png")

print(adf.test(acf_ts))

print(kpss.test(acf_ts))

gdpModel <- arima(acf_ts, method="ML")
print(Box.test(gdpModel$residuals))
summary(gdpModel)