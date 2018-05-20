# Title     : TODO
# Objective : TODO
# Created by: adityaprasann
# Created on: 19/5/18
# Read data from CSV file
SANDPFiles <- read.csv("Unit5AssignmentData.csv")
# Load data to a data frame
sandpdf = data.frame(SANDPFiles$Date,SANDPFiles$GSPC.Adjusted,SANDPFiles$PEP.Adjusted,SANDPFiles$JPM.Adjusted,
SANDPFiles$BLK.Adjusted,SANDPFiles$PFE.Adjusted,SANDPFiles$C.Adjusted,SANDPFiles$MCO.Adjusted,
SANDPFiles$GOOGL.Adjusted,SANDPFiles$WHR.Adjusted,SANDPFiles$NFLX.Adjusted)

#create a symbol map for ease of lookup
symbols <- list('Dates','GSPC','PEP','JPM','BLK','PFE', 'C','MCO','GOOGL','WHR','NFLX')

count <- 2

#daily returns data frame for each index/stock
returndf <- data.frame( diff(sandpdf[, count     ]) / sandpdf[, count     ][-length(sandpdf[, count]     )],
                        diff(sandpdf[, (count+1 )]) / sandpdf[, (count+1 )][-length(sandpdf[, (count+1 )])],
                        diff(sandpdf[, (count+2 )]) / sandpdf[, (count+2 )][-length(sandpdf[, (count+2 )])],
                        diff(sandpdf[, (count+3 )]) / sandpdf[, (count+3 )][-length(sandpdf[, (count+3 )])],
                        diff(sandpdf[, (count+4 )]) / sandpdf[, (count+4 )][-length(sandpdf[, (count+4 )])],
                        diff(sandpdf[, (count+5 )]) / sandpdf[, (count+5 )][-length(sandpdf[, (count+5 )])],
                        diff(sandpdf[, (count+6 )]) / sandpdf[, (count+6 )][-length(sandpdf[, (count+6 )])],
                        diff(sandpdf[, (count+7 )]) / sandpdf[, (count+7 )][-length(sandpdf[, (count+7 )])],
                        diff(sandpdf[, (count+8 )]) / sandpdf[, (count+8 )][-length(sandpdf[, (count+8 )])],
                        diff(sandpdf[, (count+9 )]) / sandpdf[, (count+9 )][-length(sandpdf[, (count+9 )])])
# assign header to each column in monreturndf
names(returndf) <- c(symbols[[count]][1], symbols[[count+1]][1], symbols[[count+2]][1], symbols[[count+3]][1], symbols[[count+4]][1], symbols[[count+5]][1]
, symbols[[count+6]][1], symbols[[count+7]][1], symbols[[count+8]][1], symbols[[count+9]][1])

test.mean <- 0
pvalues <- list()
#print("The monthly return of S & P 500 index and its 12 random stocks is  :")
#print(returndf)

print("********************************************S And P 500*************************************************")
print(summary(returndf$GSPC))
t <- (mean(returndf$GSPC)-test.mean)/(sd(returndf$GSPC)/sqrt(length(returndf$GSPC)-1))
p <- 2*pt(-abs(t),df=length(returndf$GSPC)-2)
pvalues[[1]] = ('SP500'=p)

print("********************************************PEP*********************************************************")
print(summary(returndf$PEP))
t <- (mean(returndf$PEP)-test.mean)/(sd(returndf$PEP)/sqrt(length(returndf$PEP)-1))
p <- 2*pt(-abs(t),df=length(returndf$PEP)-2)
pvalues[[2]] = ('PEP'=p)

print("********************************************JPM*********************************************************")
print(summary(returndf$JPM))
t <- (mean(returndf$JPM)-test.mean)/(sd(returndf$JPM)/sqrt(length(returndf$JPM)-1))
p <- 2*pt(-abs(t),df=length(returndf$JPM)-2)
pvalues[[3]] = ('JPM'=p)

print("********************************************BLK*********************************************************")
print(summary(returndf$BLK))
t <- (mean(returndf$BLK)-test.mean)/(sd(returndf$BLK)/sqrt(length(returndf$BLK)-1))
p <- 2*pt(-abs(t),df=length(returndf$BLK)-2)
pvalues[[4]] = ('BLK'=p)

print("********************************************PFE*********************************************************")
print(summary(returndf$PFE))
t <- (mean(returndf$PFE)-test.mean)/(sd(returndf$PFE)/sqrt(length(returndf$PFE)-1))
p <- 2*pt(-abs(t),df=length(returndf$PFE)-2)
pvalues[[5]] = ('PFE'=p)

print("********************************************C***********************************************************")
print(summary(returndf$C))
t <- (mean(returndf$C)-test.mean)/(sd(returndf$C)/sqrt(length(returndf$C)-1))
p <- 2*pt(-abs(t),df=length(returndf$C)-2)
pvalues[[6]] = ('C'=p)

print("********************************************MCO*********************************************************")
print(summary(returndf$MCO))
t <- (mean(returndf$MCO)-test.mean)/(sd(returndf$MCO)/sqrt(length(returndf$MCO)-1))
p <- 2*pt(-abs(t),df=length(returndf$MCO)-2)
pvalues[[7]] = ('MCO'=p)

print("********************************************GOOGL*******************************************************")
print(summary(returndf$GOOGL))
t <- (mean(returndf$GOOGL)-test.mean)/(sd(returndf$GOOGL)/sqrt(length(returndf$GOOGL)-1))
p <- 2*pt(-abs(t),df=length(returndf$GOOGL)-2)
pvalues[[8]] = ('GOOGL'=p)

print("********************************************WHR*********************************************************")
print(summary(returndf$WHR))
t <- (mean(returndf$WHR)-test.mean)/(sd(returndf$WHR)/sqrt(length(returndf$WHR)-1))
p <- 2*pt(-abs(t),df=length(returndf$WHR)-2)
pvalues[[9]] = ('WHR'=p)

print("********************************************NFLX********************************************************")
print(summary(returndf$NFLX))
t <- (mean(returndf$NFLX)-test.mean)/(sd(returndf$NFLX)/sqrt(length(returndf$NFLX)-1))
p <- 2*pt(-abs(t),df=length(returndf$NFLX)-2)
pvalues[[10]] = ('NFLX'=p)

print("********************************************************************************************************")
print("The calulated p - value of stocks are ")
print(pvalues)
print("********************************************************************************************************")



