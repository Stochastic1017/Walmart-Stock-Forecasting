
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

# Create data frame for test statistics
ARMA_summary <- data.frame(
    "Model" = c(
    "MA1",
    "Standard Error",
    "sigma2",
    "Log-Likelihood",
    "AIC",
    "AICc",
    "BIC"
  ),
   "ARIMA(0,0,1) with zero mean" = c(
    arma_fit$var.coef[1],
    sqrt(diag(arma_fit$var.coef)),
    arma_fit$sigma2,
    arma_fit$loglik,
    arma_fit$aic,
    arma_fit$aicc,
    arma_fit$bic
  )
)

# Format the table using kable
kable(ARMA_summary, 
      format = "latex",
      digits = 4,
      align = c('l', 'r', 'r')) %>%
  kable_styling(full_width = FALSE)