# Session -> Set Working Directory -> To Source File Location

library("forecast")

# ---------------------------------------
# Read in, manipulate, and visualize data
# ---------------------------------------

Coke.data <- read.csv("Coke.csv")
sales.train <- Coke.data$Sales[1:38]
sales.test <- Coke.data$Sales[39:42]
sales.ts <- ts(sales.train, start = c(1986,1), freq = 4)
plot(sales.ts)


# ----------------------------------
# Fit an exponential smoothing model
# ----------------------------------

(sales.ets <- ets(sales.ts, model = "MMM", restrict = FALSE))
summary(sales.ets)  # View the model.
sales.ets.pred <- forecast(sales.ets, h = 4, level = c(0.9))
accuracy(sales.ets.pred, sales.test)

# Plot with prediction intervals.
par(mfrow = c(1, 1))  # Set the plot frame to a one-plot panel.
plot(sales.ets.pred, ylim = c(1000, 6000), ylab = "Sales", xlab = "Year-Quarter")
lines(sales.ets$fitted, lty = 3)
points(ts(sales.test, start = c(1995, 3), freq = 4), pch = 4)

# Plot without prediction intervals.
par(mfrow = c(1, 1))
plot(sales.train, type = "l", ylim = c(1000, 6000), xlim = c(1, 42), ylab = "Sales", xlab = "Quarter", main = "ETS(M,M,M)")
lines(seq(1, 38, 1), sales.ets$fitted, lty = 3)
points(seq(39, 42, 1), sales.ets.pred$mean, pch = 16)
points(seq(39, 42, 1), sales.test, pch = 4)

# --------------------------------------
# Fit Regression Model 1: Additive Trend
# --------------------------------------

t <- seq(1, 38, 1)
sales.lm.1 <- lm(sales.train ~ t)
summary(sales.lm.1)
newdata.1 <- data.frame(t = seq(39, 42, 1))  # new data for prediction.
sales.lm.1.pred <- forecast(sales.lm.1, newdata.1)  # forecast takes model and newdata.
accuracy(sales.lm.1.pred, sales.test)

# Plot with prediction intervals.
plot(sales.lm.1.pred, lty = 3, ylim = c(1000, 6000), xlim = c(1, 42), ylab = "Sales", xlab = "Quarter")
points(t(newdata.1), sales.test, pch = 4)

# Plot without prediction intervals.
plot(sales.train, type = "l", ylim = c(1000, 6000), xlim = c(1, 42), ylab = "Sales", xlab = "Quarter")
lines(sales.lm.1.pred$fitted, lty = 3)
points(t(newdata.1), sales.lm.1.pred$mean, pch = 16)
points(t(newdata.1), sales.test, pch = 4)


# --------------------------------------------
# Fit Regression Model 2: Multiplicative Trend
# --------------------------------------------

sales.lm.2 <- lm(log(sales.train) ~ t)
summary(sales.lm.2)  # View the model.
sales.lm.2.pred <- forecast(sales.lm.2, newdata.1)
accuracy(sales.lm.2.pred, log(sales.test))  # Accuracy of predictions of log(sales).
accuracy(exp(sales.lm.2.pred$mean), sales.test)  # Accuracy of predictions of sales.

plot(sales.train, type = "l", ylim = c(1000, 6000), xlim = c(1, 42), ylab = "Sales", xlab = "Quarter")
lines(exp(sales.lm.2.pred$fitted), lty = 3)
points(t(newdata.1), exp(sales.lm.2.pred$mean), pch = 16)
points(t(newdata.1), sales.test, pch = 4)


# ------------------------------------------------------
# Fit Regression Model 3: Additive Trend and Seasonality
# ------------------------------------------------------

D2 <- I(Coke.data$Quarter[1:38] == 2)  # Dummy variable for Quarter 2.
D3 <- I(Coke.data$Quarter[1:38] == 3)  # Dummy variable for Quarter 2.
D4 <- I(Coke.data$Quarter[1:38] == 4)  # Dummy variable for Quarter 2.
sales.lm.3 <- lm(sales.train ~ t + D2 + D3 + D4)
summary(sales.lm.3)  # View the model.
newdata.3 <- data.frame(t = seq(39, 42, 1), 
                        D2 = I(Coke.data$Quarter[39:42] == 2), 
                        D3 = I(Coke.data$Quarter[39:42] == 3), 
                        D4 = I(Coke.data$Quarter[39:42] == 4))  # new data for prediction.
sales.lm.3.pred <- forecast(sales.lm.3, newdata.3)  # forecast takes model and newdata.
accuracy(sales.lm.3.pred, sales.test)

plot(sales.train, type = "l", ylim = c(1000, 6000), xlim = c(1, 42), ylab = "Sales", xlab = "Quarter")
lines(sales.lm.3.pred$fitted, lty = 3)
points(t(newdata.1), sales.lm.3.pred$mean, pch = 16)
points(t(newdata.1), sales.test, pch = 4)


# ------------------------------------------------------------
# Fit Regression Model 4: Multiplicative Trend and Seasonality
# ------------------------------------------------------------

sales.lm.4 <- lm(log(sales.train) ~ t + D2 + D3 + D4)
summary(sales.lm.4)  # View the model.
sales.lm.4.pred <- forecast(sales.lm.4, newdata.3)
accuracy(sales.lm.4.pred, log(sales.test))  # Accuracy of predictions of log(sales).
accuracy(exp(sales.lm.4.pred$mean), sales.test)  # Accuracy of predictions of sales (sort of).

plot(sales.train, type = "l", ylim = c(1000, 6000), xlim = c(1, 42), ylab = "Sales", xlab = "Quarter")
lines(exp(sales.lm.4.pred$fitted), lty = 3)
points(t(newdata.1), exp(sales.lm.4.pred$mean), pch = 16)
points(t(newdata.1), sales.test, pch = 4)


# -------------------------------------------
# Make a Final Comparison of the Above Models
# -------------------------------------------

list(Regression Additive_Trend = accuracy(sales.lm.1.pred, sales.test),
     Regression_Multiplicative_Trend = accuracy(exp(sales.lm.2.pred$mean), sales.test),
     Regression_Additive_Trend_and_Seasonality = accuracy(sales.lm.3.pred, sales.test),
     Regression_Mulitiplicative_Trend_and_Seasonality = accuracy(exp(sales.lm.4.pred$mean), sales.test),
     ETS_MMM = accuracy(sales.ets.pred, sales.test))

# Forecast plots from regression models.
par(mfrow = c(2, 2))  # Set the plot frame to a 2-by-2 panel.
# Model 1
plot(sales.train, type = "l", ylim = c(1000, 6000), xlim = c(1, 42), ylab = "Sales", xlab = "Quarter", main = "Additive Trend")
lines(sales.lm.1.pred$fitted, lty = 3)
points(t(newdata.1), sales.lm.1.pred$mean, pch = 16)
points(t(newdata.1), sales.test, pch = 4)
# Model 2
plot(sales.train, type = "l", ylim = c(1000, 6000), xlim = c(1, 42), ylab = "Sales", xlab = "Quarter", main = "Multiplicative Trend")
lines(exp(sales.lm.2.pred$fitted), lty = 3)
points(t(newdata.1), exp(sales.lm.2.pred$mean), pch = 16)
points(t(newdata.1), sales.test, pch = 4)
# Model 3
plot(sales.train, type = "l", ylim = c(1000, 6000), xlim = c(1, 42), ylab = "Sales", xlab = "Quarter", main = "Additive Trend and Seasonality")
lines(sales.lm.3.pred$fitted, lty = 3)
points(t(newdata.1), sales.lm.3.pred$mean, pch = 16)
points(t(newdata.1), sales.test, pch = 4)
# Model 4
plot(sales.train, type = "l", ylim = c(1000, 6000), xlim = c(1, 42), ylab = "Sales", xlab = "Quarter", main = "Multiplicative Trend and Seasonality")
lines(exp(sales.lm.4.pred$fitted), lty = 3)
points(t(newdata.1), exp(sales.lm.4.pred$mean), pch = 16)
points(t(newdata.1), sales.test, pch = 4)

# Residual plots
par(mfrow = c(2, 2))  # Set the plot frame to a 2-by-2 panel.
plot(sales.lm.1$residuals, type = "l", ylim = c(-1000, 1000), xlim = c(1, 42), ylab = "Residual", xlab = "Quarter", main = "Additive Trend")
plot(sales.train - exp(sales.lm.2.pred$fitted), type = "l", ylim = c(-1000, 1000), xlim = c(1, 42), ylab = "Residual", xlab = "Quarter", main = "Multiplicative Trend")
plot(sales.lm.3$residuals, type = "l", ylim = c(-1000, 1000), xlim = c(1, 42), ylab = "Sales", xlab = "Quarter", main = "Additive Trend and Seasonality")
plot(sales.train - exp(sales.lm.2.pred$fitted), type = "l", ylim = c(-1000, 1000), xlim = c(1, 42), ylab = "Sales", xlab = "Quarter", main = "Multiplicative Trend and Seasonality")

# Histograms of residuals
par(mfrow = c(2, 2))  # Set the plot frame to a 2-by-2 panel.
hist(sales.lm.1$residuals, ylim = c(0, 20), xlim = c(-1000, 1000), xlab = "Residual", main = "Additive Trend")
hist(sales.train - exp(sales.lm.2.pred$fitted), ylim = c(0, 20), xlim = c(-1000, 1000), xlab = "Residual", main = "Multiplicative Trend")
hist(sales.lm.3$residuals, ylim = c(0, 20), xlim = c(-1000, 1000), xlab = "Residual", main = "Additive Trend and Seasonality")
hist(sales.train - exp(sales.lm.2.pred$fitted), ylim = c(0, 20), xlim = c(-1000, 1000), xlab = "Residual", main = "Multiplicative Trend and Seasonality")
