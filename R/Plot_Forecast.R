# Clean the environment
rm(list = ls())

# Load required libraries
if (!require("fGarch")) install.packages("fGarch", dependencies = TRUE)
if (!require("forecast")) install.packages("forecast", dependencies = TRUE)
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
if (!require("gridExtra")) install.packages("gridExtra", dependencies = TRUE)
if (!require("quantmod")) install.packages("quantmod", dependencies = TRUE)
if (!require("dplyr")) install.packages("dplyr", dependencies = TRUE)

library(fGarch)
library(forecast)
library(ggplot2)
library(gridExtra)
library(quantmod)
library(lubridate)
library(dplyr)

# Load and preprocess data
WMT_df <- na.omit(read.csv("Walmart_AdjPrice.csv"))
WMT_df$Date <- as.Date(WMT_df$Date, format = "%Y-%m-%d")

# Calculate log returns
WMT_df$LogReturns <- c(NA, diff(log(WMT_df$AdjClose)))
WMT_df <- na.omit(WMT_df)

# Initialize cleaned data
data_cleaned <- WMT_df$LogReturns

# Threshold for influential points (standardized residuals)
threshold <- 3

# Iterative process to remove influential points
iteration <- 1
repeat {
  cat("\nIteration:", iteration, "\n")
  
  # Fit ARMA+GARCH(1,1) model
  arma_garch_model <- garchFit(~arma(0,0,1) + garch(1,1), 
                               data = data_cleaned, 
                               cond.dist = "std", 
                               trace = FALSE)
  
  # Extract standardized residuals
  residuals_model <- residuals(arma_garch_model, standardize = TRUE)
  
  # Identify influential points
  influential_points <- which(abs(residuals_model) > threshold)
  cat("Number of influential points detected:", length(influential_points), "\n")
  
  # Stop if no more influential points are found
  if (length(influential_points) == 0) {
    cat("No more influential points. Stopping iteration.\n")
    break
  }
  
  # Remove influential points and refit
  data_cleaned <- data_cleaned[-influential_points]
  cat("Removed influential points at indices:", influential_points, "\n")
  
  iteration <- iteration + 1
}

# Fit ARMA+GARCH(1,1) model
arma_garch_model <- garchFit(~arma(0,0,1) + garch(1,1), 
                             data = WMT_df$LogReturns, 
                             cond.dist = "std", 
                             trace = FALSE)

# Model summary
print(summary(arma_garch_model))

# Iteration: 1 
# Number of influential points detected: 12 
# Removed influential points at indices: 21 29 109 146 149 265 518 579 626 641 705 957 
# 
# Iteration: 2 
# Number of influential points detected: 4 
# Removed influential points at indices: 28 448 572 618 
# 
# Iteration: 3 
# Number of influential points detected: 3 
# Removed influential points at indices: 28 29 620 
# 
# Iteration: 4 
# Number of influential points detected: 1 
# Removed influential points at indices: 632 
# 
# Iteration: 5 
# Number of influential points detected: 0 
# No more influential points. Stopping iteration.
# 
# Title:
#   GARCH Modelling 
# 
# Call:
#   garchFit(formula = ~arma(0, 0, 1) + garch(1, 1), data = WMT_df$LogReturns, 
#            cond.dist = "std", trace = FALSE) 
# 
# Mean and Variance Equation:
#   data ~ arma(0, 0, 1) + garch(1, 1)
# <environment: 0x5dc0b46bd2a8>
#   [data = WMT_df$LogReturns]
# 
# Conditional Distribution:
#   std 
# 
# Coefficient(s):
#   mu       omega      alpha1       beta1       shape  
# 5.1609e-04  1.5621e-05  1.2949e-01  7.8984e-01  4.0690e+00  
# 
# Std. Errors:
#   based on Hessian 
# 
# Error Analysis:
#   Estimate  Std. Error  t value Pr(>|t|)    
# mu     5.161e-04   3.238e-04    1.594  0.11099    
# omega  1.562e-05   6.889e-06    2.267  0.02336 *  
#   alpha1 1.295e-01   4.767e-02    2.716  0.00661 ** 
#   beta1  7.898e-01   6.978e-02   11.320  < 2e-16 ***
#   shape  4.069e+00   5.061e-01    8.040 8.88e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Log Likelihood:
#   2926.143    normalized:  3.019755 
# 
# Description:
#   Fri Dec 13 16:06:17 2024 by user:  
#   
#   
#   Standardised Residuals Tests:
#   Statistic   p-Value
# Jarque-Bera Test   R    Chi^2  1.333435e+04 0.0000000
# Shapiro-Wilk Test  R    W      8.692446e-01 0.0000000
# Ljung-Box Test     R    Q(10)  6.421147e+00 0.7787270
# Ljung-Box Test     R    Q(15)  7.394690e+00 0.9457582
# Ljung-Box Test     R    Q(20)  1.103563e+01 0.9452933
# Ljung-Box Test     R^2  Q(10)  1.158213e+00 0.9996639
# Ljung-Box Test     R^2  Q(15)  1.835535e+00 0.9999833
# Ljung-Box Test     R^2  Q(20)  2.088292e+00 0.9999998
# LM Arch Test       R    TR^2   1.372843e+00 0.9999190
# 
# Information Criterion Statistics:
#   AIC       BIC       SIC      HQIC 
# -6.029191 -6.004029 -6.029244 -6.019613 
# 
# 
# Title:
#   GARCH Modelling 
# 
# Call:
#   garchFit(formula = ~arma(0, 0, 1) + garch(1, 1), data = WMT_df$LogReturns, 
#            cond.dist = "std", trace = FALSE) 
# 
# Mean and Variance Equation:
#   data ~ arma(0, 0, 1) + garch(1, 1)
# <environment: 0x5dc0b46bd2a8>
#   [data = WMT_df$LogReturns]
# 
# Conditional Distribution:
#   std 
# 
# Coefficient(s):
#   mu       omega      alpha1       beta1       shape  
# 5.1609e-04  1.5621e-05  1.2949e-01  7.8984e-01  4.0690e+00  
# 
# Std. Errors:
#   based on Hessian 
# 
# Error Analysis:
#   Estimate  Std. Error  t value Pr(>|t|)    
# mu     5.161e-04   3.238e-04    1.594  0.11099    
# omega  1.562e-05   6.889e-06    2.267  0.02336 *  
#   alpha1 1.295e-01   4.767e-02    2.716  0.00661 ** 
#   beta1  7.898e-01   6.978e-02   11.320  < 2e-16 ***
#   shape  4.069e+00   5.061e-01    8.040 8.88e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Log Likelihood:
#   2926.143    normalized:  3.019755 
# 
# Description:
#   Fri Dec 13 16:06:17 2024 by user:  
#   
#   
#   Standardised Residuals Tests:
#   Statistic   p-Value
# Jarque-Bera Test   R    Chi^2  1.333435e+04 0.0000000
# Shapiro-Wilk Test  R    W      8.692446e-01 0.0000000
# Ljung-Box Test     R    Q(10)  6.421147e+00 0.7787270
# Ljung-Box Test     R    Q(15)  7.394690e+00 0.9457582
# Ljung-Box Test     R    Q(20)  1.103563e+01 0.9452933
# Ljung-Box Test     R^2  Q(10)  1.158213e+00 0.9996639
# Ljung-Box Test     R^2  Q(15)  1.835535e+00 0.9999833
# Ljung-Box Test     R^2  Q(20)  2.088292e+00 0.9999998
# LM Arch Test       R    TR^2   1.372843e+00 0.9999190
# 
# Information Criterion Statistics:
#   AIC       BIC       SIC      HQIC 
# -6.029191 -6.004029 -6.029244 -6.019613

# Forecast parameters
forecast_length <- 10
last_date <- max(WMT_df$Date)
last_price <- tail(WMT_df$AdjClose, 1)
last_log_return <- tail(WMT_df$LogReturns, 1)

# Forecast log returns
forecasts <- predict(arma_garch_model, n.ahead = forecast_length, plot = FALSE)

# Create forecast dataframe
forecast_df <- data.frame(
  Date = seq(last_date + 1, by = 1, length.out = forecast_length),
  LogReturns = forecasts$meanForecast,
  Lower95 = forecasts$meanForecast - 1.96 * forecasts$standardDeviation,
  Upper95 = forecasts$meanForecast + 1.96 * forecasts$standardDeviation
)

# Calculate prices and confidence intervals
forecast_df$AdjPrice <- last_price * exp(cumsum(forecast_df$LogReturns))
forecast_df$LowerAdjPrice <- last_price * exp(cumsum(forecast_df$Lower95))
forecast_df$UpperAdjPrice <- last_price * exp(cumsum(forecast_df$Upper95))

# Fetch actual recent prices
getSymbols("WMT", from = last_date, 
           to = last_date + forecast_length, 
           auto.assign = TRUE)

# Prepare actual prices dataframe
actual_prices <- data.frame(
  Date = index(WMT),
  AdjClose = as.numeric(Ad(WMT))
)
actual_prices$LogReturns <- c(NA, diff(log(actual_prices$AdjClose)))
actual_prices <- na.omit(actual_prices)

# Prepare recent historical data
recent_historical_data <- tail(WMT_df, 30)

# Prepare connecting lines for price
connect_price <- data.frame(
  Date = c(last_date, forecast_df$Date[1], last_date, actual_prices$Date[1]),
  AdjClose = c(last_price, forecast_df$AdjPrice[1], last_price, actual_prices$AdjClose[1]),
  Connector = c("Forecast Connector", "Forecast Connector", "Historical Connector", "Historical Connector")
)

# Prepare connecting lines for log returns
connect_log_returns <- data.frame(
  Date = c(last_date, forecast_df$Date[1], last_date, actual_prices$Date[1]),
  LogReturns = c(last_log_return, forecast_df$LogReturns[1], last_log_return, actual_prices$LogReturns[1]),
  Connector = c("Forecast Connector", "Forecast Connector", "Historical Connector", "Historical Connector")
)

# Convert to date
connect_price$Date <- as.Date(connect_price$Date)
connect_log_returns$Date <- as.Date(connect_log_returns$Date)

palette <- c(
  "Historical" = "#79b9e7",
  "Forecast" = "#FFC220",
  "Actual" = "#0071CE",
  "Forecast Connector" = "#FFD700",  # Yellow
  "Historical Connector" = "#1E90FF" # Blue
)
# Price Plot
price_plot <- ggplot() +
  # Historical price line
  geom_line(data = recent_historical_data, 
            aes(x = Date, y = AdjClose, color = "Historical"), size = 1) +
  # Forecast price line
  geom_line(data = forecast_df, 
            aes(x = Date, y = AdjPrice, color = "Forecast"), size = 1) +
  # Actual market prices line
  geom_line(data = actual_prices, 
            aes(x = Date, y = AdjClose, color = "Actual"), size = 1) +
  # Connecting lines (with legend suppressed)
  geom_line(data = connect_price, 
            aes(x = Date, y = AdjClose, color = Connector, linetype = Connector), 
            size = 1, show.legend = FALSE) +
  # Confidence interval ribbon
  geom_ribbon(data = forecast_df, 
              aes(x = Date, ymin = LowerAdjPrice, ymax = UpperAdjPrice), 
              fill = "#FFC220", alpha = 0.2) +
  scale_color_manual(values = palette,
                     name = "Legend",
                     breaks = c("Historical", "Forecast", "Actual")) +
  scale_linetype_manual(values = c("Forecast Connector" = "solid", 
                                   "Historical Connector" = "solid")) +
  labs(
    x = "Date",
    y = "Adjusted Closing Price"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom",
  )

# Log Returns Plot
log_return_plot <- ggplot() +
  # Historical log returns
  geom_line(data = recent_historical_data, 
            aes(x = Date, y = LogReturns, color = "Historical"), size = 1) +
  # Forecast log returns
  geom_line(data = forecast_df, 
            aes(x = Date, y = LogReturns, color = "Forecast"), size = 1) +
  # Actual market log returns
  geom_line(data = actual_prices, 
            aes(x = Date, y = LogReturns, color = "Actual"), size = 1) +
  # Connecting lines (with legend suppressed)
  geom_line(data = connect_log_returns, 
            aes(x = Date, y = LogReturns, color = Connector, linetype = Connector), 
            size = 1, show.legend = FALSE) +
  # Confidence interval ribbon
  geom_ribbon(data = forecast_df, 
              aes(x = Date, ymin = Lower95, ymax = Upper95), 
              fill = "#FFC220", alpha = 0.2) +
  scale_color_manual(values = palette,
                     name = "Legend",
                     breaks = c("Historical", "Forecast", "Actual")) +
  scale_linetype_manual(values = c("Forecast Connector" = "solid", 
                                   "Historical Connector" = "solid")) +
  labs(
    x = "Date",
    y = "Log Adjusted Returns"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom",
  )

# Combine and save plots
forecast_plot <- grid.arrange(log_return_plot, price_plot, ncol = 1)
ggsave("Forecast.png", plot = forecast_plot, 
       width = 12, height = 10, dpi = 300)

# Align forecast_df and actual_prices by Date
aligned_data <- inner_join(forecast_df, actual_prices, by = "Date", suffix = c("_forecast", "_actual"))

# Calculate RMSE for log returns
log_returns_rmse <- sqrt(mean((aligned_data$LogReturns_forecast - aligned_data$LogReturns_actual)^2, na.rm = TRUE))
cat("RMSE for Log Returns: ", log_returns_rmse, "\n")

# Calculate RMSE for adjusted prices
price_rmse <- sqrt(mean((aligned_data$AdjPrice - aligned_data$AdjClose)^2, na.rm = TRUE))
cat("RMSE for Adjusted Prices: ", price_rmse, "\n")

# RMSE for Log Returns:  0.01044659 
# RMSE for Adjusted Prices:  1.16299
