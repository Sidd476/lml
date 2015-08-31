# Session -> Set Working Directory -> To Source File Location

library("forecast")
library("caret")

# -----------------
# Data Manipulation 
# -----------------

Amtrak.df <- read.csv("Amtrak data.csv")
nPeriods <- length(Amtrak.df$Ridership)
nTrain <- nPeriods - 12

# Set up the model matrices.
ytrain <- Amtrak.df$Ridership[13:nTrain]
xtrain <- data.frame(
  lag1 = Amtrak.df$Ridership[12:(nTrain - 1)],
  lag2 = Amtrak.df$Ridership[11:(nTrain - 2)],
  lag3 = Amtrak.df$Ridership[10:(nTrain - 3)],
  lag4 = Amtrak.df$Ridership[9:(nTrain - 4)],
  lag5 = Amtrak.df$Ridership[8:(nTrain - 5)],
  lag6 = Amtrak.df$Ridership[7:(nTrain - 6)],
  lag7 = Amtrak.df$Ridership[6:(nTrain - 7)],
  lag8 = Amtrak.df$Ridership[5:(nTrain - 8)],
  lag9 = Amtrak.df$Ridership[4:(nTrain - 9)],
  lag10 = Amtrak.df$Ridership[3:(nTrain - 10)],
  lag11 = Amtrak.df$Ridership[2:(nTrain - 11)],
  lag12 = Amtrak.df$Ridership[1:(nTrain - 12)]  
  )

ytest <- Amtrak.df$Ridership[(nTrain + 1):nPeriods]
xtest <- data.frame(
  lag1 = Amtrak.df$Ridership[(nTrain):(nPeriods - 1)],
  lag2 = Amtrak.df$Ridership[(nTrain - 1):(nPeriods - 2)],
  lag3 = Amtrak.df$Ridership[(nTrain - 2):(nPeriods - 3)],
  lag4 = Amtrak.df$Ridership[(nTrain - 3):(nPeriods - 4)],
  lag5 = Amtrak.df$Ridership[(nTrain - 4):(nPeriods - 5)],
  lag6 = Amtrak.df$Ridership[(nTrain - 5):(nPeriods - 6)],
  lag7 = Amtrak.df$Ridership[(nTrain - 6):(nPeriods - 7)],
  lag8 = Amtrak.df$Ridership[(nTrain - 7):(nPeriods - 8)],
  lag9 = Amtrak.df$Ridership[(nTrain - 8):(nPeriods - 9)],
  lag10 = Amtrak.df$Ridership[(nTrain - 9):(nPeriods - 10)],
  lag11 = Amtrak.df$Ridership[(nTrain - 10):(nPeriods - 11)],
  lag12 = Amtrak.df$Ridership[(nTrain - 11):(nPeriods - 12)]
)

# Normalize xtrain and xtest.
xtrain.preProcess <- preProcess(xtrain, method = c('range'))
xtrain.normalized <- predict(xtrain.preProcess, xtrain)
summary(xtrain.normalized)  # Notice how all the data are between 0 and 1.

xtest.preProcess <- preProcess(xtest, method = c('range'))
xtest.normalized <- predict(xtest.preProcess, xtest)
summary(xtest.normalized)  # Notice how all the data are between 0 and 1.


# --------------------
# Neural Net -- avNNet
# --------------------

# One way to fit a neural net is with the avNNet function. The main tuning parameters for the neural net in this function are size (the number of nodes in the hidden layer) and decay (the regularization parameter).  The fewer the nodes and the greater the decay; the less danger there is of overfitting.  Repeats is the number of neural nets to train up for ensembling. 

#  We use the settings for size, decay, and repeats given in Hyndman and Athanasopoulos's book Forecasting Principles and Practice (which is free available online at https://www.otexts.org/fpp).  In the Advanced Forecasting Methods chapeter (Chap. 9/3), they use size = 3, decay = 0.1, and repeats = 25.

set.seed(201)
ridership.nn <- avNNet(xtrain.normalized, ytrain, size = 3, decay = 0.1, repeats = 25, maxit = 2000, linout = TRUE)  
ridership.nn.fitted <- predict(ridership.nn, xtrain.normalized)
ridership.nn.pred <- predict(ridership.nn, xtest.normalized)
accuracy(ridership.nn.fitted, ytrain)
accuracy(ridership.nn.pred, ytest)

# Figure 7.12
plot(ts(ytrain, start= c(1992, 1), freq = 12), ylab = "Ridership", ylim = c(1300, 2300), main = "Neural Net Fitted vs. Actuals in Training Set")
lines(ts(ridership.nn.fitted, start = c(1992, 1), freq = 12), lty = 2)
legend(1994, 2300, c("Actual", "Forecast"), col = c("black", "black"), lty = c(1, 2), bty = "n", horiz = FALSE)
plot(ts(ytrain - ridership.nn.fitted, start = c(1992, 1), freq = 12), lty = 2, ylab = "Residuals", main = "Neural Net Residuals")  # Residual plot
plot(ts(ytest, start = c(2003, 4), freq = 12), ylab = "Ridership", ylim = c(1400, 2200), main = "Neural Net Predictions in the Testing Set")
lines(ts(ridership.nn.pred, start = c(2003, 4), freq = 12), lty = 2)

# Compare to ETS.
ridership.ets <- ets(Amtrak.df$Ridership[1:nTrain], model = "ZZZ", restrict = FALSE)  
summary(ridership.ets)
ridership.ets.pred <- forecast(ridership.ets, h = 12)
plot(ridership.ets.pred)
points(seq(nTrain + 1, nPeriods, 1), ytest)
accuracy(ridership.ets.pred, ytest)

# --------------------
# Neural Net -- nnetar
# --------------------

# The function nnetar in the forecast package automatically uses the past observations in a neural network.  It chooses the number of non-seasonal lags p based on the best-fit AR(p) model.  The number of seasonal lags P is set to 1 as its default.  The number of hidden nodes is set to half the number of inputs nodes plus one.  The number of networks ensembled is set to 20.  

# Note that the nnetar function cannot handle covariates (or external information). One of the advantages of using the avNNet model above is that you can include covariates (or external information) in the neural network.  Also, unlike ARIMA models, neural networks (modeled with either the avNNet or nnetar function) do not need the analyzed time series to be stationary.

ytrain.ts <- ts(Amtrak.df$Ridership[1:nTrain], start = c(1991, 1), freq = 12)
(ridership.nnetar <- nnetar(ytrain.ts, P=4))  # It chooses to fit 20 NNAR(p = 13, P = 1).
summary(ridership.nnetar$model[[1]])  # The first network's weights.
ridership.nnetar.pred <- forecast(ridership.nnetar, h = 12)
accuracy(ridership.nnetar.pred, ytest)  # The test set accuracy of the nnetar model is improved over the avNNet model above.  This improvement may be because the avNNet model above did not include a seasonal lag (i.e., it has P = 0). 
plot(ridership.nnetar.pred)
plot(ridership.nnetar.pred, xlim = c(2003 + 4/12, 2004 + 3/12))
lines(ts(ytest, start = c(2003, 4), freq = 12), lty = 1)


