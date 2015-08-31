# Session -> Set Working Directory -> To Source File Location

library("forecast")

Amtrak.df <- read.csv("Amtrak data.csv")
ridership.ts <- ts(Amtrak.df$Ridership, start = c(1991, 1), freq = 12)
nPeriods <- length(ridership.ts)
nTrain <- nPeriods - 12
train.ts <- window(ridership.ts, start = c(1991, 1), end = c(1991, nTrain))
test.ts <- window(ridership.ts, start = c(1991, nTrain + 1), end = c(1991, nPeriods))


# ------------------------------
# Regression Model: Linear Trend 
# ------------------------------

ridership.lm <- tslm(train.ts ~ trend)
summary(ridership.lm)
summary(ridership.lm)$coefficients
ridership.lm.pred <- forecast(ridership.lm, h = 12)
plot(ridership.lm.pred, ylab = "Ridership (in thousands)", xlab = "Time", main = "")
lines(ridership.lm$fitted)
points(test.ts, pch = 18)
plot(ridership.lm$residual, ylab = "Residuals")
accuracy(ridership.lm.pred, test.ts)


# -----------------------------------
# Regression Model: Exponential Trend 
# -----------------------------------

ridership.lm <- tslm(train.ts ~ trend, lambda = 0)
summary(ridership.lm)
summary(ridership.lm)$coefficients
ridership.lm.pred <- forecast(ridership.lm, h = 12)
plot(ridership.lm.pred, ylab = "Ridership (in thousands)", xlab = "Time", main = "")
lines(ridership.lm$fitted)
points(test.ts, pch = 18)
plot(ridership.lm$residual, ylab = "Residuals")
accuracy(ridership.lm.pred, test.ts)


# ----------------------------------
# Regression Model: Polynomial Trend 
# ----------------------------------

trend.squared <- seq(1, nTrain, 1)^2
ridership.lm <- tslm(train.ts ~ trend + trend.squared)
summary(ridership.lm)
summary(ridership.lm)$coefficients
newXs <- data.frame("trend" = seq(nTrain + 1, nPeriods, 1), "trend.squared" = seq(nTrain + 1, nPeriods, 1)^2)
ridership.lm.pred <- forecast(ridership.lm, newdata = newXs, h = 12)
plot(ridership.lm.pred, ylab = "Ridership (in thousands)", xlab = "Time", main = "")
lines(ridership.lm$fitted)
points(test.ts, pch = 18)
plot(ridership.lm$residual, ylab = "Residuals")
accuracy(ridership.lm.pred, test.ts)


# --------------------------------------
# Regression Model: Additive Seasonality 
# --------------------------------------

ridership.lm <- tslm(train.ts ~ season)
summary(ridership.lm)
summary(ridership.lm)$coefficients
newXs <- data.frame(seasonaldummy(test.ts))
ridership.lm.pred <- forecast(ridership.lm, newdata = newXs, h = 12)
par(mfrow = c(2, 1))
plot(ridership.lm.pred, ylab = "Ridership (in thousands)", xlab = "Time", main = "")
lines(ridership.lm$fitted, lty = 2)
points(test.ts, pch = 18)
plot(ridership.lm$residual, ylab = "Residuals")
accuracy(ridership.lm.pred, test.ts)

pdf("ReplaceFig5-12.pdf")
par(mfrow = c(2, 1))
plot(ridership.lm.pred, ylab = "Ridership (in thousands)", xlab = "Time", main = "")
lines(ridership.lm$fitted, lty = 2)
points(test.ts, pch = 18)
plot(ridership.lm$residual, ylab = "Residuals")
dev.off()

# ----------------------------------------------
# Regression Model: Additive Trend + Seasonality 
# ----------------------------------------------

ridership.lm <- tslm(train.ts ~ trend + season)
summary(ridership.lm)
summary(ridership.lm)$coefficients
newXs <- data.frame(seasonaldummy(test.ts))
ridership.lm.pred <- forecast(ridership.lm, newdata = newXs, h = 12)
plot(ridership.lm.pred, ylab = "Ridership (in thousands)", xlab = "Time", main = "")
lines(ridership.lm$fitted)
points(test.ts, pch = 18)
plot(ridership.lm$residual, ylab = "Residuals")
accuracy(ridership.lm.pred, test.ts)


