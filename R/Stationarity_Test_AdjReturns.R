
rm(list = ls()) # init

if (!require("ggplot2")) {
  install.packages("ggplot2")
  stopifnot(require("ggplot2"))
}

if (!require("gridExtra")) {
  install.packages("gridExtra")
  stopifnot(require("gridExtra"))
}

if (!require("dplyr")) {
  install.packages("dplyr")
  stopifnot(require("dplyr"))
}

if (!require("scales")) {
  install.packages("scales")
  stopifnot(require("scales"))
}

if (!require("forecast")) { 
  install.packages("forecast") 
  stopifnot(require("forecast"))
}

if (!require("fGarch")) {
  install.packages("fGarch")
  stopifnot(require("fGarch"))
}

if (!require("glue")) {
  install.packages("glue")
  stopifnot(require("glue"))
}

if (!require("tseries")) {
  install.packages("tseries")
  stopifnot(require("tseries"))
}

if (!require("knitr")) {
  install.packages("knitr")
  stopifnot(require("knitr"))
}

if (!require("kableExtra")) {
  install.packages("kableExtra")
  stopifnot(require("kableExtra"))
}

# Load csv
WMT_df <- na.omit(read.csv("Walmart_AdjPrice.csv"))

# Convert Date column to Date type
WMT_df$Date <- as.Date(WMT_df$Date, format = "%Y-%m-%d")

# Extract log returns
log_returns <- WMT_df$LogReturns
abs_log_return <- abs(log_returns)
squared_log_return <- log_returns^2

# Create ACF plots with dynamic titles
acf_log_returns <- ggAcf(log_returns, lag.max = 200) +
  ggtitle(glue("ACF of Log Returns")) +
  theme_minimal()

acf_abs_log_returns <- ggAcf(abs_log_return, lag.max = 200) +
  ggtitle(glue("ACF of Absolute Log Returns")) +
  theme_minimal()

acf_squared_log_returns <- ggAcf(squared_log_return, lag.max = 200) +
  ggtitle(glue("ACF of Squared Log Returns")) +
  theme_minimal()

# Combine plots using gridExtra::grid.arrange
combined_plot <- grid.arrange(
  grobs = list(acf_log_returns, acf_abs_log_returns, acf_squared_log_returns),
  ncol = 1 # Arrange plots in a single column
)

# Save combined plot to file
ggsave("ACF_Plots.png", plot = combined_plot, 
       width = 10, height = 12, dpi = 300)

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

# Create data frame for test statistics
stats_table <- data.frame(
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

# Format the table using kable
kable(stats_table, 
      format = "latex",
      col.names = c("Sample Statistic", "t-statistic", "p-value"),
      digits = 4,
      align = c('l', 'r', 'r')) %>%
  kable_styling(full_width = FALSE)
