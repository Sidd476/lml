library("forecast")

# -----------------------------------
# Deaths from Pneumonia and Influenza 
# -----------------------------------

PI.data <- read.csv("PI data.csv")  # National Center for Health Services data
death.ts <- ts(PI.data$Percent.of.Deaths.Due.to.P.I, start = c(2009, 40), freq = 365.25/7)
threshold <- ts(PI.data$Threshold, start = c(2009, 40), freq = 365.25/7)
nPeriods <- length(death.ts)

trend.squared <- seq(1, nPeriods, 1)^2
sin.term <- sin(2 * pi * seq(1, nPeriods, 1) / 52.18)
cos.term <- cos(2 * pi * seq(1, nPeriods, 1) / 52.18)
death.lm <- tslm(death.ts ~ trend + trend.squared + sin.term + cos.term)
summary(death.lm)
summary(death.lm)$coefficients
plot(death.ts, ylab = "Percent of All Deaths Due to P&I", xlab = "Time", main = "", ylim = c(6, 12))
lines(death.lm$fitted)
lines(threshold, col = "red")
legend(2011, 12, c("Baseline", "Threshold"), col = c("black", "red"), lty = c(1, 1), bty = "n", horiz = TRUE)
