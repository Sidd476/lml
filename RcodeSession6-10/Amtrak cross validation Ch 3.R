# install.packages("devtools")
# library(devtools)
# find_rtools()
# dev_mode(on=T)
# install_github("robjhyndman/forecast")
# when finished do:
# dev_mode(on=F)
# use forecast 5.9 from github now

library("forecast")

Amtrak.data <- read.csv("Amtrak data.csv")
ridership.ts <- ts(Amtrak.data$Ridership, start = c(1991,1), freq = 12)
nTotal <- length(ridership.ts)
nTrain <- nTotal - 36
kStepsAhead <- 1 
nStopRoll <- nTotal - kStepsAhead
rmse <- matrix(0, nStopRoll - nTrain + 1, 4)

for(i in nTrain:nStopRoll) {
  train.ts <- window(ridership.ts, start = c(1991, 1), end = c(1991, i))
  test.ts <- window(ridership.ts, start = c(1991, i + 1), end = c(1991, i + kStepsAhead))
  ets.1 <- ets(train.ts, model = "ANA")
  ets.1.pred <- forecast(ets.1, h = kStepsAhead)  
  ets.2 <- ets(train.ts, model = "MNM")
  ets.2.pred <- forecast(ets.2, h = kStepsAhead)
#  tsp(test.ts)[1] <- tsp(ets.1.pred$mean)[1]  # the accuracy function in Version 5.8 from CRAN needs the exact same start for the forecasts and testing set.  Version 5.9 from github (Feb 1, 2015) does not.
  rmse[i - nTrain + 1, 1] <- accuracy(ets.1.pred, test.ts)[2, 2]
  rmse[i - nTrain + 1, 2] <- accuracy(ets.2.pred, test.ts)[2, 2]
  rmse[i - nTrain + 1, 3] <- accuracy(naive(train.ts, h = kStepsAhead), test.ts)[2, 2]
  rmse[i - nTrain + 1, 4] <- accuracy(snaive(train.ts, h = kStepsAhead), test.ts)[2, 2]
}

rmse
results <- data.frame(t(colMeans(rmse)))
colnames(results) <- c("ETS(A,N,A)", "ETS(M,N,M)", "Naive", "SNaive")
results


