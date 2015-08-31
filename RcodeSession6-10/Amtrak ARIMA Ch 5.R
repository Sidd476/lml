# In RStudio, Session > Set Working Directory > To Source File Location

library("forecast")


# -----------------
# Data Manipulation
# -----------------

Amtrak.df <- read.csv("Amtrak data.csv")
ridership.ts <- ts(Amtrak.df$Ridership, start = c(1991, 1), freq = 12)
ridership.ts.24.months <- window(ridership.ts, start = c(1991, 1), end = c(1992, 12))
nPeriods <- length(ridership.ts)
nTrain <- nPeriods - 12
train.ts <- window(ridership.ts, start = c(1991, 1), end = c(1991, nTrain))
test.ts <- window(ridership.ts, start = c(1991, nTrain + 1), end = c(1991, nPeriods))


# ----------------
# ACF of Ridership
# ----------------

Acf(ridership.ts.24.months, lag.max = 12, main = "ACF Plot of Ridership")


# ---------------------------------------------------------------
# ACF of Residuals from Polynomial Trend + Seasonality Regression
# ---------------------------------------------------------------

trend.squared <- seq(1, nTrain, 1)^2
ridership.lm <- tslm(train.ts ~ trend + trend.squared + season)
Acf(ridership.lm$residuals, lag.max = 12, main = "ACF Plot of Regression Residuals")
(res.arima <- Arima(ridership.lm$residuals, order = c(1, 0, 0)))
res.arima.pred <- forecast(res.arima, h = 12)
plot(ridership.lm$residuals, ylab = "Ridership Residuals (in thousands)", xlab = "Time", main = "")
lines(res.arima.pred$fitted, col = "red")


# ---------------------------------------------------
# ACF of Residuals from ARIMA on Regression Residuals
# ---------------------------------------------------

Acf(res.arima$residuals, lag.max = 12, main = "ACF Plot of ARIMA Residuals")



