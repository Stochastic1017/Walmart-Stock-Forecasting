
# Clean the environment
rm(list = ls())

# Load required libraries
if (!require("fGarch")) install.packages("fGarch", dependencies = TRUE)
if (!require("forecast")) install.packages("forecast", dependencies = TRUE)
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
if (!require("gridExtra")) install.packages("gridExtra", dependencies = TRUE)

library(fGarch)
library(forecast)
library(ggplot2)
library(gridExtra)

# Load CSV
WMT_df <- na.omit(read.csv("Walmart_AdjPrice.csv"))

# Convert Date column to Date type
WMT_df$Date <- as.Date(WMT_df$Date, format = "%Y-%m-%d")

# Fit ARMA+GARCH(1,1) model
arma_garch_model <- garchFit(~arma(0,0,1) + garch(1,1), 
                             data=WMT_df$LogReturns, cond.dist="std")

# Identify influential points (e.g., standardized residuals > 3)
influential_points <- which(abs(residuals(arma_garch_model, 
                                          standardize = TRUE)) > 3)
# Load the dataset and preprocess
WMT_df <- na.omit(read.csv("Walmart_AdjPrice.csv"))
WMT_df$Date <- as.Date(WMT_df$Date, format = "%Y-%m-%d")

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

# Final model with cleaned data
cleaned_arma_garch_model <- garchFit(~arma(0,0,1) + garch(1,1), 
                                     data = data_cleaned, 
                                     cond.dist = "std", 
                                     trace = FALSE)

# Model summary
print(summary(cleaned_arma_garch_model))

# Iteration: 1 
# Number of influential points detected: 12 
# Removed influential points at indices: 22 30 110 147 150 266 519 580 627 642 706 958 
# 
# Iteration: 2 
# Number of influential points detected: 4 
# Removed influential points at indices: 29 449 573 619 
# 
# Iteration: 3 
# Number of influential points detected: 3 
# Removed influential points at indices: 29 30 621 
# 
# Iteration: 4 
# Number of influential points detected: 1 
# Removed influential points at indices: 633 
# 
# Iteration: 5 
# Number of influential points detected: 0 
# No more influential points. Stopping iteration.
# 
# Title:
#   GARCH Modelling 
# 
# Call:
#   garchFit(formula = ~arma(0, 0, 1) + garch(1, 1), data = data_cleaned, 
#            cond.dist = "std", trace = FALSE) 
# 
# Mean and Variance Equation:
#   data ~ arma(0, 0, 1) + garch(1, 1)
# <environment: 0x5faaadbb60c0>
#   [data = data_cleaned]
# 
# Conditional Distribution:
#   std 
# 
# Coefficient(s):
#   mu       omega      alpha1       beta1       shape  
# 4.9198e-04  6.3505e-06  1.0224e-01  8.5001e-01  1.0000e+01  
# 
# Std. Errors:
#   based on Hessian 
# 
# Error Analysis:
#   Estimate  Std. Error  t value Pr(>|t|)    
# mu     4.920e-04   3.213e-04    1.531  0.12572    
# omega  6.350e-06   3.262e-06    1.947  0.05158 .  
# alpha1 1.022e-01   3.102e-02    3.296  0.00098 ***
#   beta1  8.500e-01   4.877e-02   17.430  < 2e-16 ***
#   shape  1.000e+01   2.507e+00    3.988 6.66e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Log Likelihood:
#   2972.232    normalized:  3.128665 
# 
# Description:
#   Tue Nov 26 15:47:15 2024 by user:  
#   
#   
#   Standardised Residuals Tests:
#   Statistic    p-Value
# Jarque-Bera Test   R    Chi^2   3.355696 0.18677549
# Shapiro-Wilk Test  R    W       0.996212 0.02083633
# Ljung-Box Test     R    Q(10)   4.340417 0.93068634
# Ljung-Box Test     R    Q(15)   8.318518 0.91037890
# Ljung-Box Test     R    Q(20)  17.592676 0.61422106
# Ljung-Box Test     R^2  Q(10)   5.779881 0.83340515
# Ljung-Box Test     R^2  Q(15)   8.138288 0.91811888
# Ljung-Box Test     R^2  Q(20)  12.309297 0.90501891
# LM Arch Test       R    TR^2    5.568408 0.93625874
# 
# Information Criterion Statistics:
#   AIC       BIC       SIC      HQIC 
# -6.246803 -6.221243 -6.246858 -6.237065 
# 
# 
# Title:
#   GARCH Modelling 
# 
# Call:
#   garchFit(formula = ~arma(0, 0, 1) + garch(1, 1), data = data_cleaned, 
#            cond.dist = "std", trace = FALSE) 
# 
# Mean and Variance Equation:
#   data ~ arma(0, 0, 1) + garch(1, 1)
# <environment: 0x5faaadbb60c0>
#   [data = data_cleaned]
# 
# Conditional Distribution:
#   std 
# 
# Coefficient(s):
#   mu       omega      alpha1       beta1       shape  
# 4.9198e-04  6.3505e-06  1.0224e-01  8.5001e-01  1.0000e+01  
# 
# Std. Errors:
#   based on Hessian 
# 
# Error Analysis:
#   Estimate  Std. Error  t value Pr(>|t|)    
# mu     4.920e-04   3.213e-04    1.531  0.12572    
# omega  6.350e-06   3.262e-06    1.947  0.05158 .  
# alpha1 1.022e-01   3.102e-02    3.296  0.00098 ***
#   beta1  8.500e-01   4.877e-02   17.430  < 2e-16 ***
#   shape  1.000e+01   2.507e+00    3.988 6.66e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Log Likelihood:
#   2972.232    normalized:  3.128665 
# 
# Description:
#   Tue Nov 26 15:47:15 2024 by user:  
#   
#   
#   Standardised Residuals Tests:
#   Statistic    p-Value
# Jarque-Bera Test   R    Chi^2   3.355696 0.18677549
# Shapiro-Wilk Test  R    W       0.996212 0.02083633
# Ljung-Box Test     R    Q(10)   4.340417 0.93068634
# Ljung-Box Test     R    Q(15)   8.318518 0.91037890
# Ljung-Box Test     R    Q(20)  17.592676 0.61422106
# Ljung-Box Test     R^2  Q(10)   5.779881 0.83340515
# Ljung-Box Test     R^2  Q(15)   8.138288 0.91811888
# Ljung-Box Test     R^2  Q(20)  12.309297 0.90501891
# LM Arch Test       R    TR^2    5.568408 0.93625874
# 
# Information Criterion Statistics:
#   AIC       BIC       SIC      HQIC 
# -6.246803 -6.221243 -6.246858 -6.237065 

# Extract standardized residuals for the cleaned model
residuals_cleaned <- residuals(cleaned_arma_garch_model, standardize=TRUE)

# 1. Residuals over time
residuals_time <- data.frame(Time=1:length(residuals_cleaned), 
                             Residuals=residuals_cleaned)
p1 <- ggplot(residuals_time, aes(x=Time, y=Residuals)) +
  geom_point(alpha=0.6) +
  geom_hline(yintercept=0, color="red", linetype="dashed", linewidth=1) +
  labs(title="Residuals Over Time", x="Time", y="Standardized Residuals") +
  theme_minimal()

# 2. Residual histogram with KDE and theoretical distribution
p2 <- ggplot(data.frame(Residuals = residuals_cleaned), aes(x = Residuals)) +
  geom_histogram(aes(y = ..density..), 
                 bins=30, fill="lightblue", color="black") +
  geom_density(color = "blue", linetype = "solid", linewidth=1) +
  stat_function(fun=dnorm, args=list(mean=mean(residuals_cleaned), 
                                     sd=sd(residuals_cleaned)), 
                color="red", linetype="solid", linewidth=1) +
  labs(title = "Histogram of Residuals with KDE", x = "Residuals", y = "Density") +
  theme_minimal()

# Q-Q Plot of residuals
p3 <- ggplot(data.frame(Residuals = residuals_cleaned), aes(sample = Residuals)) +
  stat_qq() +
  stat_qq_line(color = "red", linewidth=1) +
  labs(title = "Q-Q Plot of Residuals", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()

# Influential points graph (standardized residuals > 3 or < -3)
influential_points_cleaned <- which(abs(residuals_cleaned) > 3)
influential_residuals <- residuals_time[influential_points_cleaned, ]

p4 <- ggplot(residuals_time, aes(x = Time, y = Residuals)) +
  geom_line() +
  geom_hline(yintercept = c(-3, 3), color = "red", 
             linetype = "dashed", linewidth=1) +
  labs(title = "Influential Points in Residuals", x = "Time", y = "Standardized Residuals") +
  theme_minimal()

# Display the plots
residual_plots <- grid.arrange(p1, p2, 
                               p3, p4, ncol=2, nrow=2)

# Save the combined plot as a PNG file
ggsave("Residual_Diagnostics.png", plot=residual_plots, 
       width=10, height=8, dpi=300)

# ACF plots (Residuals, Squared Residuals, Absolute Residuals)
acf_residuals_plot <- ggAcf(residuals_cleaned, lag.max=200) +
  ggtitle("ACF of Residuals") +
  theme_minimal()

# ACF of squared residuals
acf_squared_residuals_plot <- ggAcf(residuals_cleaned^2, lag.max=200) +
  ggtitle("ACF of Squared Residuals") +
  theme_minimal()

# ACF of absolute residuals
acf_abs_residuals_plot <- ggAcf(abs(residuals_cleaned), lag.max=200) +
  ggtitle("ACF of Absolute Residuals") +
  theme_minimal()

# Display the plots
ACF_plots <- grid.arrange(acf_residuals_plot, 
                          acf_squared_residuals_plot, 
                          acf_abs_residuals_plot, ncol = 1)

# Save the combined plot as a PNG file
ggsave("ACF_plots.png", plot = ACF_plots, width = 10, height = 8, dpi = 300)
