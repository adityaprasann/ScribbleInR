# Title     : TODO
# Objective : TODO
# Created by: adityaprasann
# Created on: 3/5/18
# 1. Consider the following values for this project:
# a. St = 10$
# b. r= 0.15 (15% expected return per year) c. σ =0.20 (20% annual volatility in prices) d. T = 1 year
# e. N=100
# f. ε =0.15
# 2. Starting with the initial stock price St as specified, and considering 100 Steps, calculate the expected value of the stock price at the end of every successive Δt interval of time
# 3. Plot the entire movement of prices over the T period under observation
# 4. Instead of considering a fixed ε as in the previous steps, randomly assign values to ε from a standard normal distribution.
# 5. Perform 5 trials of 100 steps each to plot the probable movement of stock prices over a 1-year period. Plot each trajectory of prices as a separate line
St = 10
r = 0.15
sig = 0.20
N = 100
eps = 0.15
a <- 1 : 99
b <- 1 : 5
xdata <- 1 : 100
stockprice <- numeric(length = length(a))
stockprice[1] = St
stockpricelist <- list(length(b))
colorlist <- list("red", "blue", "green", "black", "yellow")
pchlist <- list("*", "+", "-", "#", "^")


randeps <- rnorm(5, 0.4, 0.2)
print(randeps)
for (i in seq_along(b)) {
    print(randeps[i])
    for (j in seq_along(a)) {
        stockprice[j + 1] <- stockprice[j] * exp(((r - (0.5 * (sig ^ 2))) * (1 / N)) + (sig * randeps[i] * sqrt(1 / N)))
    }
    stockpricelist[[i]] <- stockprice
}

for (i in seq_along(b))
{
    print(stockpricelist[[i]])
    if (i == 1) {
        png(file = "stockprice_chart.jpg")
        plot(xdata, stockpricelist[[i]], type = "o", xlab = "Interval", ylab = "Stock Prices", main = "Stock prices over time interval", lty = i, col = colorlist[[i]], pch = pchlist[[i]])
    } else {
        points(xdata, stockpricelist[[i]], col = colorlist[[i]], pch = pchlist[[i]])
        lines(xdata, stockpricelist[[i]], col = colorlist[[i]])
    }
}



