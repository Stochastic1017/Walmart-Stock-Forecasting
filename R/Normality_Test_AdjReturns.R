
# Clean the environment
rm(list = ls())

# Load required libraries
if (!require("moments")) install.packages("moments", dependencies = TRUE)
if (!require("knitr")) install.packages("knitr", dependencies = TRUE)
if (!require("kableExtra")) install.packages("kableExtra", dependencies = TRUE)

library(moments)
library(knitr)
library(kableExtra)

# Load csv
WMT_df <- read.csv("Walmart_AdjPrice.csv")

# Convert Date column to Date type
WMT_df$Date <- as.Date(WMT_df$Date, format = "%Y-%m-%d")

# Compute summary statistics
statistics_df <- data.frame(
  Statistic = c("Mean", 
                "Standard Deviation", 
                "Variance", 
                "Median", 
                "Minimum", 
                "Maximum", 
                "Skewness", 
                "Kurtosis", 
                "Excess Kurtosis"),
  Value = c(
    mean(na.omit(WMT_df$LogReturns)),
    sd(na.omit(WMT_df$LogReturns)),
    var(na.omit(WMT_df$LogReturns)),
    median(na.omit(WMT_df$LogReturns)),
    min(na.omit(WMT_df$LogReturns)),
    max(na.omit(WMT_df$LogReturns)),
    moments::skewness(na.omit(WMT_df$LogReturns)),
    moments::kurtosis(na.omit(WMT_df$LogReturns)),
    moments::kurtosis(na.omit(WMT_df$LogReturns)) - 3
  )
)

# Save the LaTeX table to a file
latex_table <- kable(statistics_df, format = "latex", booktabs = TRUE, digits = 4,
                     caption = "Summary Statistics for Walmart Log Returns")

# Assuming WMT_df is your data frame with the LogReturns column
log_returns <- na.omit(WMT_df$LogReturns)  # Remove NA values

# 1. Test for Mean (μ = 0)
t_test_result <- t.test(log_returns, mu = 0)
print(t_test_result)

# 2. Test for Skewness = 0
skewness_value <- skewness(log_returns)
skewness_test_stat <- skewness_value/sqrt(6/length(log_returns))
p_value_skew <- 2 * (1-pnorm(abs(skewness_test_stat)))
cat("Skewness Test: Test Statistic =", skewness_test_stat, ", p-value =", p_value_skew, "\n")

# 3. Test for Excess Kurtosis = 0
kurtosis_value <- kurtosis(log_returns) - 3  # Calculate excess kurtosis
kurtosis_test_stat <- kurtosis_value / sqrt(24 / length(log_returns))  # Test statistic for kurtosis
p_value_kurt <- 2 * (1 - pnorm(abs(kurtosis_test_stat)))  # Two-tailed p-value
cat("Kurtosis Test: Test Statistic =", kurtosis_test_stat, ", p-value =", p_value_kurt, "\n")

# 4. Combined Normality Test
shapiro_test_result <- shapiro.test(log_returns)  # Shapiro-Wilk Test

# Create data frame for normality test
normality_test <- data.frame(
  "Sample_Statistic" = c(
    sprintf("Mean = %.4f", mean(log_returns)),
    sprintf("Skewness = %.4f", skewness(log_returns)),
    sprintf("Kurtosis = %.4f", kurtosis(log_returns) - 3),
    sprintf("Shapiro-Wilk Test")
  ),
  "test statistic" = c(
    t_test_result$statistic,
    skewness_test_stat,
    kurtosis_test_stat,
    shapiro_test_result$statistic
  ),
  "p_value" = c(
    t_test_result$p.value,
    p_value_skew,
    p_value_kurt,
    shapiro_test_result$p.value
  )
)
print(normality_test)

# One Sample t-test
# 
# data:  log_returns
# t = 0.69844, df = 987, p-value = 0.4851
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#   -0.0006023636  0.0012680822
# sample estimates:
#   mean of x 
# 0.0003328593 
# 
# Skewness Test: Test Statistic = -5.161373 , p-value = 2.45145e-07 
# Kurtosis Test: Test Statistic = 89.52865 , p-value = 0 
# Sample_Statistic test.statistic      p_value
# 1      Mean = 0.0003      0.6984359 4.850692e-01
# 2 Skewness = -0.4022     -5.1613731 2.451450e-07
# 3 Kurtosis = 13.9537     89.5286453 0.000000e+00
# 4  Shapiro-Wilk Test      0.8490343 1.157196e-29

