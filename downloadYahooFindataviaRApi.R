# Title     : TODO
# Objective : TODO
# Created by: adityaprasann
# Created on: 19/5/18

library(quantmod)

StockNames <- c('^GSPC','C','JPM','GOOGL','MCO','NFLX','PEP','PFE','V','WHR','BLK','BA')
start <- ('2017-05-17')
end <- ('2018-05-16')

stocks <- new.env()

getSymbols(Symbols = StockNames,
src = "yahoo", from = "2017-05-17",to = "2018-05-16", env = stocks)
StockData <- do.call(merge, eapply(stocks, 'Ad'))

print(StockData)

write.csv(StockData, file = "MyData.csv",row.names=FALSE)
