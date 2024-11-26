
rm(list = ls()) # init

if (!require("fGarch")) { # forecast for time series analysis
  install.packages("fGarch") # install it only once
  stopifnot(require("fGarch")) # load it every time
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

# Fit fGARCH(1,1) model to log returns
fit_arch <- garchFit(~ garch(1, 0), data = WMT_df$LogReturns, cond.dist = "std")
summary(fit_arch)

fit_garch <- garchFit(~ garch(1, 1), data = WMT_df$LogReturns, cond.dist = "std")
summary(fit_garch)
