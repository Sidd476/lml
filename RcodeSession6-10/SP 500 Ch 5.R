# In RStudio, Session > Set Working Directory > To Source File Location

library("forecast")
library("tseries")


# -----------------
# Data Manipulation
# -----------------

SP500.df <- read.csv("SP 500 data (100 months).csv")
stock.ts <- ts(SP500.df$Close, start = c(1995, 5), freq = 12)
plot(stock.ts)

# -------------------------
# Evaluating Predictability 
# -------------------------

Arima(stock.ts, order = c(1, 0, 0))
(t.stat <- (0.9833 - 1) / 0.0145)
(p.value <- pt(t.stat, df = 98))  # one-sided t-test nearly rejects the null hypothesis of a random walk.
adf.test(stock.ts, alternative = "stationary")  # Better to use the Augmented Dickey Fuller Test. It does not even come close to rejecting the null hypothesis of a random walk.
