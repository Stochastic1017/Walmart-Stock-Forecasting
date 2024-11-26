
# Clean the environment
rm(list = ls())

# Load required libraries
if (!require("tseries")) install.packages("tseries", dependencies = TRUE)

library(tseries)

# Load csv
WMT_df <- na.omit(read.csv("Walmart_AdjPrice.csv"))

# Convert Date column to Date type
WMT_df$Date <- as.Date(WMT_df$Date, format = "%Y-%m-%d")

# Fit ARMA model automatically
arma_fit <- auto.arima(WMT_df$LogReturns, seasonal = FALSE)
print(summary(arma_fit))

# Series: WMT_df$LogReturns 
# ARIMA(0,0,1) with zero mean 
# 
# Coefficients:
#   ma1
# -0.0666
# s.e.   0.0312
# 
# sigma^2 = 0.0002266:  log likelihood = 2694.43
# AIC=-5384.85   AICc=-5384.84   BIC=-5375.1
# 
# Training set error measures:
#   ME       RMSE         MAE      MPE   MAPE      MASE         ACF1
# Training set 0.0003912174 0.01504515 0.009658333 96.81537 110.64 0.6897203 -0.002686485