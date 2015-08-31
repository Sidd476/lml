# Session -> Set Working Directory -> To Source File Location

library("forecast")
library("caret")

# -----------------
# Data Manipulation 
# -----------------

rain.df <- read.csv("MelbourneRainfall.csv")
rain.df$Date <- as.Date(rain.df$Date, format="%m/%d/%Y")
rain.df$Month <- as.numeric(format(rain.df$Date, "%m"))
rain.df$Year <- as.numeric(format(rain.df$Date, "%Y"))
rain.df$Rainy <- ifelse(rain.df$Rainfall > 0, 1, 0)
train.df <- rain.df[rain.df$Date <= as.Date("12/31/2009", format="%m/%d/%Y"), ]
test.df <- rain.df[rain.df$Date > as.Date("12/31/2009", format="%m/%d/%Y"), ]

nPeriods <- length(rain.df$Rainfall)
nTrain <- length(train.df$Rainfall)
nTest <- length(test.df$Rainfall)


# ------------------
# Data Visualization 
# ------------------

# Pivot table of Rainy by month by year.
(rainy.by.month.by.year <- aggregate(Rainy ~ Month + Year, data = rain.df, mean))
# Pivot table of Rainy by month.
(rainy.by.month <- aggregate(Rainy ~ Month, data = rain.df, mean))

# Figure 7.1
plot(rainy.by.month.by.year[rainy.by.month.by.year$Year == 2000, 1], rainy.by.month.by.year[rainy.by.month.by.year$Year == 2000, 3], type = "l", lty = 1, ylab = "Percent of rainy days per month", xlab = "Month", ylim = c(0.05, 0.65))
for(i in 2001:2011) {
  lines(rainy.by.month.by.year[rainy.by.month.by.year$Year == i, 1], rainy.by.month.by.year[rainy.by.month.by.year$Year == i, 3])
}
lines(rainy.by.month$Rainy, lty = 2, lwd = 2)


# ---------------------
# Binary Classification 
# ---------------------


# Design matrix.
rainfall.lag.1 <- c(0, train.df$Rainfall[1:(nTrain - 1)]
t.var <- seq(1, nTrain, 1)
sin.var = sin(2 * pi * t.var / 365.25)
cos.var = cos(2 * pi * t.var / 365.25)

# Fit a logistic regression.
rainy.lr <- glm(Rainy ~ rainfall.lag.1 + sin.var + cos.var, data = train.df, family = "binomial")
summary(rainy.lr)

# Make predictions in the test set using the logistic regression.
t.var.test <- seq(nTrain + 1, nPeriods)
xtest <- data.frame(rainfall.lag.1 = c(train.df$Rainfall[nTrain], test.df$Rainfall[1:(nTest - 1)]), sin.var = sin(2 * pi * t.var.test / 365.25), cos.var = cos(2 * pi * t.var.test / 365.25))
rainy.lr.pred <- predict(rainy.lr, xtest, type = "response") 

# Confusion matrices.
cut.off <- 0.5
confusionMatrix(ifelse(rainy.lr$fitted > cut.off, 1, 0), train.df$Rainy)
confusionMatrix(ifelse(rainy.lr.pred > cut.off, 1, 0), test.df$Rainy)

# RMSE is the square root of the Brier score, a popular binary classification score.
accuracy(rainy.lr.pred, test.df$Rainy)


