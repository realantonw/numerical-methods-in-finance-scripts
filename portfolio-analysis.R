## Numerische Methoden in der Finanzwirtschaft - Homework 1
## Anton Wohlgemuth
## 11778996



# Get quantmod
if (!require("quantmod")) {
  install.packages("quantmod")
  library(quantmod)
}

#Setting the start and end data for our Analyzes
start <- as.Date("2018-01-01")
end <- as.Date("2018-11-01")

# Selected Stocks:
# Amazon, Facebook, Tesla

con <- url("http://quote.yahoo.com")

#Getting the stock data
getSymbols("amzn", src = "yahoo", from = start, to = end)
getSymbols("fb", src = "yahoo", from = start, to = end)
getSymbols("tsla", src = "yahoo", from = start, to = end)


# Visualizing Stock Data

# Simple Linechart:
plot(AMZN$AMZN.Close, main = "Amazon")
plot(FB$FB.Close, main = "Facebook")
plot(TSLA$TSLA.Close, main = "Tesla")

# Advanced Candle Charts (quantmod)
candleChart(FB, up.col = "black", dn.col = "red", theme = "white")
candleChart(AMZN, up.col = "black", dn.col = "red", theme = "white")
candleChart(TSLA, up.col = "black", dn.col = "red", theme = "white")

# Comparing the stocks in a single plot:

stocks <- as.xts(data.frame(FB = FB$FB.Close, AMZN = AMZN$AMZN.Close, 
                            TSLA = TSLA$TSLA.Close))
head(stocks)

plot(as.zoo(stocks), screens = 1, lty = 1:3, xlab = "Date", ylab = "Price")
legend("right", c("FB", "AMZN", "TSLA"), lty = 1:3, cex = 0.5)

# Calculating the stock returns

if (!require("magrittr")) {
  install.packages("magrittr")
  library(magrittr)
}

stock_return = apply(stocks, 1, function(x) {x / stocks[1,]}) %>% 
  t %>% as.xts

head(stock_return)

# Plotting the stock returns to see how profitable each stock was.
plot(as.zoo(stock_return), screens = 1, lty = 1:3, xlab = "Date", ylab = "Return")
legend("topleft", c("FB", "AMZN", "TSLA"), lty = 1:3, cex = 0.5)

# Calculating the stock change:
stock_change = stocks %>% log %>% diff
head(stock_change)

# Plotting the stock change:
plot(as.zoo(stock_change), screens = 1, lty = 1:3, xlab = "Date", ylab = "Log Difference")
legend("topleft", c("FB", "AMZN", "TSLA"), lty = 1:3, cex = 0.5)

# Calculating the moving averages
# Moving averages smooth a series and helps identify trends.

candleChart(FB, up.col = "black", dn.col = "red", theme = "white")
#plotting the 20-day, 50-day and 200-day moving averages
addSMA(n = c(20, 50, 200))

candleChart(AMZN, up.col = "black", dn.col = "red", theme = "white")
#plotting the 20-day, 50-day and 200-day moving averages
addSMA(n = c(20, 50, 200))

candleChart(TSLA, up.col = "black", dn.col = "red", theme = "white")
# plotting the 20-day, 50-day and 200-day moving averages
addSMA(n = c(20, 50, 200))

# Based on all the charts I've plotted I would invest in TSLA. 


# Furthermore I took a look at the correlation between this 3 stocks.
# Correllation Plot:
data<-cbind(diff(log(Cl(AMZN))),diff(log(Cl(FB))),diff(log(Cl(TSLA))))
chart.Correlation(data)


# I've also caluculated the statistical measurements we discussed in the last lecture 

# ----
# TSLA
# ----

S.TSLA <- (as.numeric(TSLA$TSLA.Close))
R.TSLA <- (S.TSLA[-1] - S.TSLA[1:(length(S.TSLA)-1)])/S.TSLA[1:(length(S.TSLA)-1)]

# Erwartungswert und Varianz
mean(R.TSLA)
var(R.TSLA)

# empirische Quantile
quantile(R.TSLA)

quantile(R.TSLA, c(0.05, 0.95))

plot(density(R.TSLA))
abline(v = mean(R.TSLA), col = 2)

# Schiefekoeffizient
r.TSLA <- quantile(R.TSLA)["75%"] - quantile(R.TSLA)["50%"]
l.TSLA <- quantile(R.TSLA)["50%"] - quantile(R.TSLA)["25%"]

sk.TSLA <- (r.TSLA-l.TSLA)/(r.TSLA+l.TSLA)
#sk > 0: rechtschief: Mittelwert > Median
#sk < 0: linksschief: Mittelwert < Median

# Woelbungskoeffizient

s.TSLA <- sqrt(var(R.TSLA)) # Standardabweichung
QD.TSLA <- quantile(R.TSLA)["75%"] - quantile(R.TSLA)["25%"]# Interquartilsdistanz
wk.TSLA <- QD.TSLA/s.TSLA ## normalverteilt:

## Normalverteilt?
plot(qqnorm(R.TSLA))
qqline(R.TSLA)


## Jarque Bera Test
jarque.bera.test(R.TSLA) ## grosser p-Wert: >> normalverteilt


# ----
# FB
# ----

S.FB <- (as.numeric(FB$FB.Close))
R.FB <- (S.FB[-1] - S.FB[1:(length(S.FB)-1)])/S.FB[1:(length(S.FB)-1)]

# Erwartungswert und Varianz
mean(R.FB)
var(R.FB)

# empirische Quantile
quantile(R.FB)

quantile(R.FB, c(0.05, 0.95))

plot(density(R.FB))
abline(v = mean(R.FB), col = 2)

# Schiefekoeffizient
r.FB <- quantile(R.FB)["75%"] - quantile(R.FB)["50%"]
l.FB <- quantile(R.FB)["50%"] - quantile(R.FB)["25%"]

sk.FB <- (r.FB-l.FB)/(r.FB+l.FB)
#sk > 0: rechtschief: Mittelwert > Median
#sk < 0: linksschief: Mittelwert < Median

# Woelbungskoeffizient

s.FB <- sqrt(var(R.FB)) # Standardabweichung
QD.FB <- quantile(R.FB)["75%"] - quantile(R.FB)["25%"]# Interquartilsdistanz
wk.FB <- QD.FB/s.FB ## normalverteilt:

## Normalverteilt?
plot(qqnorm(R.FB))
qqline(R.FB)


## Jarque Bera Test
jarque.bera.test(R.FB) ## grosser p-Wert: >> normalverteilt

# ----
# AMZN
# ----

S.AMZN <- (as.numeric(AMZN$AMZN.Close))
R.AMZN <- (S.AMZN[-1] - S.AMZN[1:(length(S.AMZN)-1)])/S.AMZN[1:(length(S.AMZN)-1)]

# Erwartungswert und Varianz
mean(R.AMZN)
var(R.AMZN)

# empirische Quantile
quantile(R.AMZN)

quantile(R.AMZN, c(0.05, 0.95))

plot(density(R.AMZN))
abline(v = mean(R.AMZN), col = 2)

# Schiefekoeffizient
r.AMZN <- quantile(R.AMZN)["75%"] - quantile(R.AMZN)["50%"]
l.AMZN <- quantile(R.AMZN)["50%"] - quantile(R.AMZN)["25%"]

sk.AMZN <- (r.AMZN-l.AMZN)/(r.AMZN+l.AMZN)
#sk > 0: rechtschief: Mittelwert > Median
#sk < 0: linksschief: Mittelwert < Median

# Woelbungskoeffizient

s.AMZN <- sqrt(var(R.AMZN)) # Standardabweichung
QD.AMZN <- quantile(R.AMZN)["75%"] - quantile(R.AMZN)["25%"]# Interquartilsdistanz
wk.AMZN <- QD.AMZN/s.AMZN ## normalverteilt:

## Normalverteilt?
plot(qqnorm(R.AMZN))
qqline(R.AMZN)


## Jarque Bera Test
jarque.bera.test(R.AMZN) ## grosser p-Wert: >> normalverteilt


# Based on all the charts I've plotted I would invest in TSLA.

