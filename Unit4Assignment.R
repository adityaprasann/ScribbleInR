# Title     : Unit 4 assignment
# Objective : TODO
# Created by: adityaprasann
# Created on: 12/5/18
# For data download in 1 view pythonproj download section
#1. Downloaddataforlast3yearsfortheDJIA(DowJonesIndustrialAverage) and each of the 30 component stocks. Download data from an appropriate financial website such as Google Finance, Yahoo Finance, Quandl, CityFALCON, or another similar source. If you are using the R language, then there are videos in the "Supplemental Videos in R" located in the "Supplemental Materials" at the bottom of the course ware on how to import CSV files into your program.
#2. CalculateMonthlyreturnsoftheDJIAindexandthedownloadedstocks over the period under study
#3. CalculatemeanandstandarddeviationofmonthlyreturnsfortheDJIA index
#4. Chooseanequalweightedportfolioconsistingofany5randomstocksfrom the DJIA, calculate the mean monthly returns and its standard deviation. Do the same for portfolios of 10,15, 20 and 25 random stocks from the DJIA universe
#5. Calculatetrackingerrorsforeachoftheportfoliosi.e.themarginbywhich the mean and standard deviation of the portfolio returns diverge from those of DJIA
#6. Graphicallyrepresentthetrackingerrorforreturnsandrisk(standard deviation of returns used as a proxy for risk) on y-axis against the sample size of portfolio on the x-axis


# Read data from CSV file
DJIACFiles <- read.csv("/Users/adityaprasann/IdeaProjects/PythonProj/RLangScripts/DowJonesIndexAndComponentMonthlyClose.csv")
# Load data to a data frame
dowjonesdf = data.frame(DJIACFiles$Date,DJIACFiles$DJI,DJIACFiles$DWDP,DJIACFiles$MMM,DJIACFiles$IBM,DJIACFiles$KO,
DJIACFiles$MCD,DJIACFiles$GS,DJIACFiles$MSFT,DJIACFiles$TRV,DJIACFiles$AXP,DJIACFiles$PG,DJIACFiles$UTX,DJIACFiles$JPM,
DJIACFiles$AAPL,DJIACFiles$DIS,DJIACFiles$BA,DJIACFiles$XOM,DJIACFiles$INTC,DJIACFiles$CAT,DJIACFiles$GE,DJIACFiles$V,
DJIACFiles$NKE,DJIACFiles$CVX,DJIACFiles$CSCO,DJIACFiles$WMT,DJIACFiles$PFE,DJIACFiles$JNJ,DJIACFiles$HD,DJIACFiles$UNH,
DJIACFiles$MRK,DJIACFiles$VZ)

#create a symbol map for ease of lookup
symbols <- list('Dates','DJI','DWDP','MMM','IBM','KO','MCD','GS','MSFT','TRV','AXP','PG','UTX','JPM','AAPL','DIS','BA','XOM','INTC','CAT',
'GE','V','NKE','CVX','CSCO','WMT','PFE','JNJ','HD','UNH','MRK','VZ')

count <- 2

#monthly returns data frame for each index/stock
monreturndf <- data.frame(diff(dowjonesdf[, count     ]) / dowjonesdf[, count     ][-length(dowjonesdf[, count]     )],
                          diff(dowjonesdf[, (count+1 )]) / dowjonesdf[, (count+1 )][-length(dowjonesdf[, (count+1 )])],
                          diff(dowjonesdf[, (count+2 )]) / dowjonesdf[, (count+2 )][-length(dowjonesdf[, (count+2 )])],
                          diff(dowjonesdf[, (count+3 )]) / dowjonesdf[, (count+3 )][-length(dowjonesdf[, (count+3 )])],
                          diff(dowjonesdf[, (count+4 )]) / dowjonesdf[, (count+4 )][-length(dowjonesdf[, (count+4 )])],
                          diff(dowjonesdf[, (count+5 )]) / dowjonesdf[, (count+5 )][-length(dowjonesdf[, (count+5 )])],
                          diff(dowjonesdf[, (count+6 )]) / dowjonesdf[, (count+6 )][-length(dowjonesdf[, (count+6 )])],
                          diff(dowjonesdf[, (count+7 )]) / dowjonesdf[, (count+7 )][-length(dowjonesdf[, (count+7 )])],
                          diff(dowjonesdf[, (count+8 )]) / dowjonesdf[, (count+8 )][-length(dowjonesdf[, (count+8 )])],
                          diff(dowjonesdf[, (count+9 )]) / dowjonesdf[, (count+9 )][-length(dowjonesdf[, (count+9 )])],
                          diff(dowjonesdf[, (count+10)]) / dowjonesdf[, (count+10)][-length(dowjonesdf[, (count+10)])],
                          diff(dowjonesdf[, (count+11)]) / dowjonesdf[, (count+11)][-length(dowjonesdf[, (count+11)])],
                          diff(dowjonesdf[, (count+12)]) / dowjonesdf[, (count+12)][-length(dowjonesdf[, (count+12)])],
                          diff(dowjonesdf[, (count+13)]) / dowjonesdf[, (count+13)][-length(dowjonesdf[, (count+13)])],
                          diff(dowjonesdf[, (count+14)]) / dowjonesdf[, (count+14)][-length(dowjonesdf[, (count+14)])],
                          diff(dowjonesdf[, (count+15)]) / dowjonesdf[, (count+15)][-length(dowjonesdf[, (count+15)])],
                          diff(dowjonesdf[, (count+16)]) / dowjonesdf[, (count+16)][-length(dowjonesdf[, (count+16)])],
                          diff(dowjonesdf[, (count+17)]) / dowjonesdf[, (count+17)][-length(dowjonesdf[, (count+17)])],
                          diff(dowjonesdf[, (count+18)]) / dowjonesdf[, (count+18)][-length(dowjonesdf[, (count+18)])],
                          diff(dowjonesdf[, (count+19)]) / dowjonesdf[, (count+19)][-length(dowjonesdf[, (count+19)])],
                          diff(dowjonesdf[, (count+20)]) / dowjonesdf[, (count+20)][-length(dowjonesdf[, (count+20)])],
                          diff(dowjonesdf[, (count+21)]) / dowjonesdf[, (count+21)][-length(dowjonesdf[, (count+21)])],
                          diff(dowjonesdf[, (count+22)]) / dowjonesdf[, (count+22)][-length(dowjonesdf[, (count+22)])],
                          diff(dowjonesdf[, (count+23)]) / dowjonesdf[, (count+23)][-length(dowjonesdf[, (count+23)])],
                          diff(dowjonesdf[, (count+24)]) / dowjonesdf[, (count+24)][-length(dowjonesdf[, (count+24)])],
                          diff(dowjonesdf[, (count+25)]) / dowjonesdf[, (count+25)][-length(dowjonesdf[, (count+25)])],
                          diff(dowjonesdf[, (count+26)]) / dowjonesdf[, (count+26)][-length(dowjonesdf[, (count+26)])],
                          diff(dowjonesdf[, (count+27)]) / dowjonesdf[, (count+27)][-length(dowjonesdf[, (count+27)])],
                          diff(dowjonesdf[, (count+28)]) / dowjonesdf[, (count+28)][-length(dowjonesdf[, (count+28)])],
                          diff(dowjonesdf[, (count+29)]) / dowjonesdf[, (count+29)][-length(dowjonesdf[, (count+29)])],
                          diff(dowjonesdf[, (count+30)]) / dowjonesdf[, (count+30)][-length(dowjonesdf[, (count+30)])])

# assign header to each column in monreturndf
names(monreturndf) <- c(symbols[[count]][1], symbols[[count+1]][1], symbols[[count+2]][1], symbols[[count+3]][1], symbols[[count+4]][1], symbols[[count+5]][1]
, symbols[[count+6]][1], symbols[[count+7]][1], symbols[[count+8]][1], symbols[[count+9]][1], symbols[[count+10]][1], symbols[[count+11]][1], symbols[[count+12]][1]
, symbols[[count+13]][1], symbols[[count+14]][1], symbols[[count+15]][1], symbols[[count+16]][1], symbols[[count+17]][1], symbols[[count+18]][1], symbols[[count+19]][1]
, symbols[[count+20]][1], symbols[[count+21]][1], symbols[[count+22]][1], symbols[[count+23]][1], symbols[[count+24]][1], symbols[[count+25]][1]
, symbols[[count+26]][1], symbols[[count+27]][1], symbols[[count+28]][1], symbols[[count+29]][1], symbols[[count+30]][1])

print("The monthly return of Dow Jones index and its components is  :")
print(monreturndf)

print("*********************************************************************************************")

print("The mean of monthly return of Dow Jones index is  :")
DJIAreturns.mean = mean(monreturndf$DJI, na.rm = TRUE)
print(DJIAreturns.mean)

print("The standard deviation of monthly return of Dow Jones index is  :")
DJIAreturns.sd = sd(monreturndf$DJI, na.rm = TRUE)
print(DJIAreturns.sd)
print("*********************************************************************************************")

rand5index <- sample(3:31, 5, replace=F)
print("The 5 random stocks selected are  :")
print(str(symbols[rand5index+1]))
rand5returns <- monreturndf[, rand5index]
#print(rand5returns)
rand5.eqwght.rowmean <- rowMeans(rand5returns)
#print(rand5.eqwght.rowmean)
print("The mean of random 5 stock is  :")
rand5.eqwght.mean = mean(rand5.eqwght.rowmean, na.rm = TRUE)
print(rand5.eqwght.mean)
print("The standard deviation of random 5 stock is  :")
rand5.eqwght.sd = sd(rand5.eqwght.rowmean, na.rm = TRUE)
print(rand5.eqwght.sd)
print("The tracking error of random 5 stock is  :")
rand5.trackingerror <- sqrt((sum((rand5.eqwght.rowmean - monreturndf$DJI)^2, na.rm = TRUE) * (12/36)))
print(rand5.trackingerror)
print("*********************************************************************************************")

rand10index <- sample(3:31, 10, replace=F)
print("The 10 random stocks selected are  :")
print(str(symbols[rand10index+1]))
rand10returns <- monreturndf[, rand10index]
#print(rand10returns)
rand10.eqwght.rowmean <- rowMeans(rand10returns)
#print(rand10.eqwght.rowmean)
print("The mean of random 10 stock is  :")
rand10.eqwght.mean = mean(rand10.eqwght.rowmean, na.rm = TRUE)
print(rand10.eqwght.mean)
print("The standard deviation of random 10 stock is  :")
rand10.eqwght.sd = sd(rand10.eqwght.rowmean, na.rm = TRUE)
print(rand10.eqwght.sd)
print("The tracking error of random 10 stock is  :")
rand10.trackingerror <- sqrt((sum((rand10.eqwght.rowmean - monreturndf$DJI)^2, na.rm = TRUE) * (12/36)))
print(rand10.trackingerror)
print("*********************************************************************************************")

rand15index <- sample(3:31, 15, replace=F)
print("The 15 random stocks selected are  :")
print(str(symbols[rand15index+1]))
rand15returns <- monreturndf[, rand15index]
#print(rand15returns)
rand15.eqwght.rowmean <- rowMeans(rand15returns)
#print(rand15.eqwght.rowmean)
print("The mean of random 15 stock is  :")
rand15.eqwght.mean = mean(rand15.eqwght.rowmean, na.rm = TRUE)
print(rand15.eqwght.mean)
print("The standard deviation of random 15 stock is  :")
rand15.eqwght.sd = sd(rand15.eqwght.rowmean, na.rm = TRUE)
print(rand15.eqwght.sd)
print("The tracking error of random 15 stock is  :")
rand15.trackingerror <- sqrt((sum((rand15.eqwght.rowmean - monreturndf$DJI)^2, na.rm = TRUE) * (12/36)))
print(rand15.trackingerror)
print("*********************************************************************************************")

rand20index <- sample(3:31, 20, replace=F)
print("The 20 random stocks selected are  :")
print(str(symbols[rand20index+1]))
rand20returns <- monreturndf[, rand20index]
#print(rand20returns)
rand20.eqwght.rowmean <- rowMeans(rand20returns)
#print(rand20.eqwght.rowmean)
print("The mean of random 20 stock is  :")
rand20.eqwght.mean = mean(rand20.eqwght.rowmean, na.rm = TRUE)
print(rand20.eqwght.mean)
print("The standard deviation of random 20 stock is  :")
rand20.eqwght.sd = sd(rand20.eqwght.rowmean, na.rm = TRUE)
print(rand20.eqwght.sd)
print("The tracking error of random 20 stock is  :")
rand20.trackingerror <- sqrt((sum((rand20.eqwght.rowmean - monreturndf$DJI)^2, na.rm = TRUE) * (12/36)))
print(rand20.trackingerror)
print("*********************************************************************************************")

rand25index <- sample(3:31, 25, replace=F)
print("The 25 random stocks selected are  :")
print(str(symbols[rand25index+1]))
rand25returns <- monreturndf[, rand25index]
#print(rand25returns)
rand25.eqwght.rowmean <- rowMeans(rand25returns)
#print(rand25.eqwght.rowmean)
print("The mean of random 25 stock is  :")
rand25.eqwght.mean = mean(rand25.eqwght.rowmean, na.rm = TRUE)
print(rand25.eqwght.mean)
print("The standard deviation of random 25 stock is  :")
rand25.eqwght.sd = sd(rand25.eqwght.rowmean, na.rm = TRUE)
print(rand25.eqwght.sd)
print("The tracking error of random 25 stock is  :")
rand25.trackingerror <- sqrt((sum((rand25.eqwght.rowmean - monreturndf$DJI)^2, na.rm = TRUE) * (12/36)))
print(rand25.trackingerror)
print("*********************************************************************************************")

trackingerr <- c(rand5.trackingerror, rand10.trackingerror, rand15.trackingerror, rand20.trackingerror, rand25.trackingerror)
randomsize <- c(5, 10, 15, 20, 25)
png(file = "trackingerror_chart.jpg")
plot(randomsize, trackingerr, type = "o", xlab = "Portfolio Size", ylab = "Tracking Error", main = "Tracking error plotted against portfolio size", col = "blue")




