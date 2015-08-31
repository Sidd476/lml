# Session -> Set Working Directory -> To Source File Location


# -------------------------------------
# Install and Load the forecast Package
# -------------------------------------

install.packages("forecast") # Run this line only once on your machine.
library("forecast") # Run this line each time you open this R file and work with it.


# ---------------------------------------
# Read-in, Format, and Visualize the Data
# ---------------------------------------

# Read in data.  Save data as csv file in Excel and in the same folder as this R file.
Amtrak.data <- read.csv("Amtrak.csv")

# Create a time series object out of the ridership column.
ridership.ts <- ts(Amtrak.data$Ridership, start = c(1991,1), freq = 12)

# Visualize the ridership over time.
plot.ts(ridership.ts, ylab = "Ridership")


# ----------------------------------------------------------------
# Fitting an Exponential Smoothing Model to the Entire Time Series
# ----------------------------------------------------------------

# Fit an ETS model to the entire ridership time series. ETS stands for error, trend, seasonality. The error can be additive (A) or multiplicative (M). The trend can be additive, multiplicative, additive damped (Ad), multiplicative damped (Md), or none (N). The seasonality can be additive, multiplicative, or none. For example, the ETS model with additive error, no trend, and additive seasonality is denoted by the model = "ANA" in the ets function. 
ridership.ets <- ets(ridership.ts, model = "ANM", restrict = FALSE)
ridership.ets  # Print the selected model to the screen. 

# Forecast h = 12 months ahead. The level will give the prediction intervals.
ridership.ets.pred <- forecast(ridership.ets, h = 12, level = c(0.5, 0.9))
ridership.ets.pred  # Print the selected predictions to the screen.

# Plot the forecast cone.
plot(ridership.ets.pred)


# ----------------------------------------------------------------------
# Fitting an Exponential Smoothing Model to Training and Validation Sets
# ----------------------------------------------------------------------

# Define a few key values, which will be used later to set up training and validation sets.  
st <- tsp(ridership.ts)[1]  # Start of the time series in time units to be used in the window function.
en <- tsp(ridership.ts)[2]  # End of the time series in time units to be used in the window function.
nPeriods <- length(ridership.ts)  # Number of periods in the time series.
nTrain <- floor(0.6 * nPeriods)  # Number of periods in the training set.

# Set up the training and validation sets. Use a 60/40 split: 95 data points in the training set, and 64 data points in the validation set.  
train.ts <- window(ridership.ts, start = st, end = st + (nTrain - 1) / 12)  # Training time series.
valid.ts <- window(ridership.ts, start = st + nTrain / 12, end = en)  # Validation time series.

# Fit an ets model to the training set and make forecasts of the actuals in the validation (or test) set.
train.ets <- ets(train.ts, model = "ANM", restrict = FALSE)
train.ets.pred <- forecast(train.ets, h = nPeriods - nTrain)

# Compare the accuracy of the ets, non-seasonal naive, and seasonal naive forecasts.
accuracy(train.ets.pred, valid.ts)
accuracy(naive(train.ts, h = nPeriods - nTrain), valid.ts)
accuracy(snaive(train.ts, h = nPeriods - nTrain), valid.ts)



