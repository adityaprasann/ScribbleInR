# Title     : TODO
# Objective : TODO
# Created by: adityaprasann
# Created on: 26/5/18
NASDAQ2YearDataFiles <- read.csv("NASDAQ2YearData.csv")
# Load data to a data frame
nasdaqdf = data.frame(NASDAQ2YearDataFiles$CSCO_Open,NASDAQ2YearDataFiles$GOOGL_Open)
#print(nasdaqdf)

#Print descriptive statistics for population
print(summary(nasdaqdf))

# Different descriptive statistics graph
png(file = "CISCO_Histogram.jpg")
hist(nasdaqdf$NASDAQ2YearDataFiles.CSCO_Open, xlab = "Range", ylab = "CISCO Stock Prices", main = "CISCO Stock prices histogram")
png(file = "CISCO_Normal.jpg")
qqnorm(nasdaqdf$NASDAQ2YearDataFiles.CSCO_Open, main = "CISCO Stock prices QQNorm")
qqline(nasdaqdf$NASDAQ2YearDataFiles.CSCO_Open)
png(file = "CISCO_BoxPlot.jpg")
boxplot(nasdaqdf$NASDAQ2YearDataFiles.CSCO_Open, main = "CISCO Stock prices boxplot")

png(file = "Google_Histogram.jpg")
hist(nasdaqdf$NASDAQ2YearDataFiles.GOOGL_Open, xlab = "Range", ylab = "Google Stock Prices", main = "Google Stock prices histogram")
png(file = "Google_Normal.jpg")
qqnorm(nasdaqdf$NASDAQ2YearDataFiles.GOOGL_Open, main = "Google Stock prices QQNorm")
qqline(nasdaqdf$NASDAQ2YearDataFiles.GOOGL_Open)
png(file = "Google_BoxPlot.jpg")
boxplot(nasdaqdf$NASDAQ2YearDataFiles.GOOGL_Open, main = "Google Stock prices boxplot")




# Take a 30 day sample for two stocks
random30daysdf <- data.frame(nasdaqdf[sample(nrow(nasdaqdf), 30), ])

#Print descriptive statistics for sample
print(summary(random30daysdf))

# Different sample descriptive statistics graph
png(file = "CISCO_Sample_Histogram.jpg")
hist(random30daysdf$NASDAQ2YearDataFiles.CSCO_Open, xlab = "Range", ylab = "CISCO Sample Stock Prices", main = "CISCO Stock prices histogram")
png(file = "CISCO_Sample_Normal.jpg")
qqnorm(random30daysdf$NASDAQ2YearDataFiles.CSCO_Open, main = "CISCO Sample Stock prices QQNorm")
qqline(random30daysdf$NASDAQ2YearDataFiles.CSCO_Open)
png(file = "CISCO_Sample_BoxPlot.jpg")
boxplot(random30daysdf$NASDAQ2YearDataFiles.CSCO_Open, main = "CISCO Sample Stock prices boxplot")

png(file = "Google_Sample_Histogram.jpg")
hist(random30daysdf$NASDAQ2YearDataFiles.GOOGL_Open, xlab = "Range", ylab = "Google Sample Stock Prices", main = "Google Stock prices histogram")
png(file = "Google_Sample_Normal.jpg")
qqnorm(random30daysdf$NASDAQ2YearDataFiles.GOOGL_Open, main = "Google Sample Stock prices QQNorm")
qqline(random30daysdf$NASDAQ2YearDataFiles.GOOGL_Open)
png(file = "Google_Sample_BoxPlot.jpg")
boxplot(random30daysdf$NASDAQ2YearDataFiles.GOOGL_Open, main = "Google Sample Stock prices boxplot")

# Do a hypopthesis testing on the sample
n1 <- length(random30daysdf$NASDAQ2YearDataFiles.CSCO_Open)
xbar1 <- mean(random30daysdf$NASDAQ2YearDataFiles.CSCO_Open)
s1 <- sd(random30daysdf$NASDAQ2YearDataFiles.CSCO_Open)

n2 <- length(random30daysdf$NASDAQ2YearDataFiles.GOOGL_Open)
xbar2 <- mean(random30daysdf$NASDAQ2YearDataFiles.GOOGL_Open)
s2 <- sd(random30daysdf$NASDAQ2YearDataFiles.GOOGL_Open)

n = min(n1,n2)
t = ((xbar1 - xbar2) - (0-0)) / sqrt(s1^2/n1 + s2^2/n2)
print(2 * (pt(abs(n), df=n-1, lower.tail=FALSE)))