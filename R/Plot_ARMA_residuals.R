
# Clean the environment
rm(list = ls())

# Load required libraries
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
if (!require("forecast")) install.packages("forecast", dependencies = TRUE)
if (!require("glue")) install.packages("glue", dependencies = TRUE)
if (!require("gridExtra")) install.packages("gridExtra", dependencies = TRUE)
if (!require("tseries")) install.packages("tseries", dependencies = TRUE)

library(ggplot2)
library(forecast)
library(glue)
library(gridExtra)
library(tseries)

# Load csv
WMT_df <- na.omit(read.csv("Walmart_AdjPrice.csv"))

# Convert Date column to Date type
WMT_df$Date <- as.Date(WMT_df$Date, format = "%Y-%m-%d")

# Fit ARMA model automatically
arma_fit <- auto.arima(WMT_df$LogReturns, seasonal = FALSE)

# Extract residuals
residuals_arma <- resid(arma_fit)
residual_box_ljung <- Box.test(residuals_arma, lag=200, type = "Ljung")
residual_shapiro_wilk <- shapiro.test(residuals_arma)

# Residuals over time
residuals_time <- data.frame(Time = 1:length(residuals_arma), 
                             Residuals = residuals_arma)

# Calculate the standard deviation of residuals
residual_sd <- sd(residuals_arma)

# Scatterplot of residuals over time
p1 <- ggplot(residuals_time, aes(x = Time, y = Residuals)) +
  geom_point(color = "black", alpha = 0.25) +                 
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  theme_light() +
  ggtitle("Residuals Over Time") +
  ylab("Residuals") +
  xlab("Time")

# Histogram of residuals with KDE and theoretical N(0, sigma^2)
p2 <- ggplot(residuals_time, aes(x = Residuals)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +  # Histogram
  geom_density(color = "red", size = 1) +                         
  stat_function(fun = dnorm, args = list(mean = 0, sd = residual_sd),                    # Theoretical N(0, sigma^2)
                color = "blue", size = 1, linetype = "solid") +
  theme_light() +
  ggtitle(glue("Histogram of Residuals")) +
  xlab("Residuals") +
  ylab("Density")

# Create ACF plot with Box-Ljung p-value in the title
p3 <- ggAcf(residuals_arma, lag.max = 200) +
  ggtitle(glue("Residual ACF (Box-Ljung p-value={round(residual_box_ljung$p.value, 3)})")) +
  theme_light()

# Q-Q plot
p4 <- ggplot(residuals_time, aes(sample = Residuals)) +
  stat_qq() +
  stat_qq_line(color = "red", linewidth=1) +
  theme_light() +
  ggtitle(glue("Q-Q Plot of Residuals (Shapiro-Wilk={round(residual_shapiro_wilk$p.value, 3)})"))

# Combine all plots using gridExtra
combined_plot <- grid.arrange(
  grobs = list(p1, p2, p3, p4),
  ncol = 2
)