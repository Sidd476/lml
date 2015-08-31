# Session -> Set Working Directory -> To Source File Location

library("forecast")
library("TTR")  # Package with trailing moving average function SMA.


# -----------------
# Data Manipulation 
# -----------------

Amtrak.df <- read.csv("Amtrak data.csv")
ridership.ts <- ts(Amtrak.df$Ridership, start = c(1991, 1), freq = 12)
nPeriods <- length(ridership.ts)
nTrain <- nPeriods - 12
train.ts <- window(ridership.ts, start = c(1991, 1), end = c(1991, nTrain))
test.ts <- window(ridership.ts, start = c(1991, nTrain + 1), end = c(1991, nPeriods))


# --------------
# Moving Average 
# --------------

ridership.cma <- ma(ridership.ts, order = 12, centre = TRUE)  # Centered moving average.
ridership.tma <- SMA(ridership.ts, n = 12)  # Trailing moving average.
plot(ridership.ts, ylab = "Ridership (in thousands)", xlab = "Time", main = "", col = "gray")
lines(ridership.cma, lty = 1)
lines(ridership.tma, lty = 2)
legend(1994, 2250, c("Ridership", "Centered MA", "Trailing MA"), col = c("gray", "black", "black"), lty = c(1, 1, 2), bty = "n", horiz = FALSE)

# Figure 6.4
train.tma <- SMA(train.ts, n = 12)
tail(train.tma, n = 1)   # Last trailing moving average in training set.
ridership.ma.pred <- rep(tail(train.tma, n = 1), 12)   # Last trailing moving average as the prediction for each of the 12 months in the testing set.
accuracy(ridership.ma.pred, test.ts)  # Testing set accuracy.
accuracy(tail(train.tma, nTrain - 12), tail(train.ts, nTrain - 12))  # Training set accuracy.

# Figure 6.5
trend.squared <- seq(1, nTrain, 1)^2
ridership.lm <- tslm(train.ts ~ trend + trend.squared + season)
newXs <- data.frame("trend" = seq(nTrain + 1, nPeriods, 1), "trend.squared" = seq(nTrain + 1, nPeriods, 1)^2, "season" = seasonaldummy(test.ts))
(ridership.lm.pred <- forecast(ridership.lm, newdata = newXs, h = 12))  # April 2003 forecast is 2114.887 thousand riders.
mean(last(ridership.lm$residual, n = 12))  # Adjust April 2003 down by 66.45.


# ----------------------------------------------------
# Detrending and Deseasonalizing Using Moving Averages 
# ----------------------------------------------------

ridership.decomp.A <- decompose(ridership.ts, type = "additive")
plot(ridership.decomp.A)
ridership.decomp.A$trend  # Centered moving average of order 12 (T_t).
head(ridership.decomp.A$seasonal, n = 12)  # Additive seasonal indices (S_t): average of y_t - T_t for each month (Jan-Dec).
hist(ridership.decomp.A$random)  # Histogram of residual y_t - T_t - S_t.
Acf(ridership.decomp.A$random)

ridership.decomp.M <- decompose(ridership.ts, type = "multiplicative")
plot(ridership.decomp.M)
ridership.decomp.M$trend  # Centered moving average of order 12 (T_t).
head(ridership.decomp.M$seasonal, n = 12)  # Multiplicative seasonal indices (S_t): average of y_t / T_t for each month (Jan-Dec).
hist(ridership.decomp.M$random)  # Histogram of residual y_t / (T_t S_t).
Acf(ridership.decomp.M$random)


# -------------------------------------------------
# Detrending and Deseasonalizing Using Differencing 
# -------------------------------------------------

ridership.lag.1 <- diff(ridership.ts, lag = 1)
ridership.lag.12 <- diff(ridership.ts, lag = 12)
ridership.lag.12.then.1 <- diff(ridership.lag.12, lag = 1)

# Figure 6.6
par(mfrow = c(2, 2))
plot(ridership.ts, ylab = "Ridership", xlab = "Time", main = "Monthly Ridership", col = "gray")
plot(ridership.lag.12, ylab = "Lag-12", xlab = "Time", main = "Lag-12 Differenced Series", col = "gray")
plot(ridership.lag.1, ylab = "Lag-1", xlab = "Time", main = "Lag-1 Differenced Series", col = "gray")
plot(ridership.lag.12.then.1, ylab = "Twice (lag-12, then lag-1)", xlab = "Time", main = "Double Differenced Series", col = "gray")


# ---------------------
# Exponential Smoothing 
# ---------------------

# Figure 6.7
res.ses <- ets(ridership.lm$residual, model = "ANN", alpha = 0.2)  # ETS(A,N,N) is SES.
res.ses.pred <- forecast(res.ses, h = 1)
par(mfrow = c(1, 1))
plot(ridership.lm$residual, ylab = "Residuals")
lines(res.ses.pred$fitted, lty = 2)
legend(1998, -150, c("Actual", "Forecast"), col = c("black", "black"), lty = c(1, 2), bty = "n", horiz = FALSE)


# ------------------------------
# Advanced Exponential Smoothing 
# ------------------------------

# Figure 6.8
ridership.ets <- ets(train.ts, model = "ANM", restrict = FALSE)  # ETS(A,N,M) is Holt-Winter's method with multiplicative seasonality and additive error.  Normally, the forecast restricts the user from running this model.  Turn off the restriction with restrict = FALSE.
summary(ridership.ets)  # Initial seasonal indices go backward from Dec-Jan.
head(ridership.decomp.M$seasonal, n = 12)  # Similar to the multiplicative indices above.
ridership.ets.pred <- forecast(ridership.ets, h = 12)
# Plot without prediction intervals.
par(mfrow = c(1, 1))
plot(ridership.ets.pred, type = "l", ylab = "Ridership", xlab = "Time", main = "Holt-Winter's Multiplicative Method")
lines(ridership.ets.pred$fitted, lty = 2)
legend(1992, 2300, c("Actual", "Forecast"), col = c("black", "black"), lty = c(1, 2), bty = "n", horiz = FALSE)

accuracy(ridership.ets.pred, test.ts)

