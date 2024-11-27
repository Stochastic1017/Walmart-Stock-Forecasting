
# Clean the environment
rm(list = ls())

# Load required libraries
if (!require("tseries")) install.packages("tseries", dependencies = TRUE)
if (!require("forecast")) install.packages("forecast", dependencies = TRUE)
if (!require("kableExtra")) install.packages("kableExtra", dependencies = TRUE)

library(tseries)      # For ADF test
library(forecast)     # For KPSS test
library(kableExtra)   # For formatting tables

# Load csv
WMT_df <- na.omit(read.csv("Walmart_AdjPrice.csv"))

# Convert Date column to Date type
WMT_df$Date <- as.Date(WMT_df$Date, format = "%Y-%m-%d")

# Extract log returns
log_returns <- WMT_df$LogReturns
abs_log_return <- abs(log_returns)
squared_log_return <- log_returns^2

# Perform Box-Ljung tests
box_ljung_log <- Box.test(log_returns, lag = 200, type = "Ljung-Box")
box_ljung_abs <- Box.test(abs_log_return, lag = 200, type = "Ljung-Box")
box_ljung_squared <- Box.test(squared_log_return, lag = 200, type = "Ljung-Box")

# Perform ADF tests
adf_log <- adf.test(log_returns, alternative = "stationary")
adf_abs <- adf.test(abs_log_return, alternative = "stationary")
adf_squared <- adf.test(squared_log_return, alternative = "stationary")

# Perform KPSS tests
kpss_log <- kpss.test(log_returns, null = "Level")
kpss_abs <- kpss.test(abs_log_return, null = "Level")
kpss_squared <- kpss.test(squared_log_return, null = "Level")

# Create data frame for stationarity test
stationarity_test <- data.frame(
  "Test" = c(
    "Box-Ljung Test (Log Returns)", "Box-Ljung Test (Absolute Log Returns)", "Box-Ljung Test (Squared Log Returns)",
    "ADF Test (Log Returns)", "ADF Test (Absolute Log Returns)", "ADF Test (Squared Log Returns)",
    "KPSS Test (Log Returns)", "KPSS Test (Absolute Log Returns)", "KPSS Test (Squared Log Returns)"
  ),
  "Test Statistic" = c(
    box_ljung_log$statistic, box_ljung_abs$statistic, box_ljung_squared$statistic,
    adf_log$statistic, adf_abs$statistic, adf_squared$statistic,
    kpss_log$statistic, kpss_abs$statistic, kpss_squared$statistic
  ),
  "P-Value" = c(
    box_ljung_log$p.value, box_ljung_abs$p.value, box_ljung_squared$p.value,
    adf_log$p.value, adf_abs$p.value, adf_squared$p.value,
    kpss_log$p.value, kpss_abs$p.value, kpss_squared$p.value
  )
)
print(stationarity_test)

# Test Test.Statistic      P.Value
# 1          Box-Ljung Test (Log Returns)   215.37365191 2.168258e-01
# 2 Box-Ljung Test (Absolute Log Returns)   616.38461461 0.000000e+00
# 3  Box-Ljung Test (Squared Log Returns)   368.80467547 3.842926e-12
# 4                ADF Test (Log Returns)   -10.38538774 1.000000e-02
# 5       ADF Test (Absolute Log Returns)    -7.05432016 1.000000e-02
# 6        ADF Test (Squared Log Returns)    -7.76192028 1.000000e-02
# 7               KPSS Test (Log Returns)     0.04139522 1.000000e-01
# 8      KPSS Test (Absolute Log Returns)     0.84312763 1.000000e-02
# 9       KPSS Test (Squared Log Returns)     0.52944444 3.503504e-02

