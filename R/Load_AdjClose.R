
rm(list = ls())

# Install required libraries
if (!require("quantmod")) install.packages("quantmod", dependencies = TRUE)
if (!require("fBasics")) install.packages("fBasics", dependencies = TRUE)
if (!require("fGarch")) install.packages("fGarch", dependencies = TRUE)
if (!require("timeSeries")) install.packages("timeSeries", dependencies = TRUE)
if (!require("zoo")) install.packages("zoo", dependencies = TRUE)
if (!require("reactable")) install.packages("reactable", dependencies = TRUE)

library(quantmod)
library(fBasics)
library(fGarch)
library(timeSeries)
library(zoo)
library(reactable)

#################
### Loading Data
#################

getSymbols("WMT", from = "2020-01-01", to = "2023-12-06") 
AdjClose = Ad(WMT) # Adjusted Close Prices

# Create a data frame with adjusted close prices and log returns
WMT_df <- data.frame(
  Date = index(AdjClose),
  AdjClose = coredata(AdjClose),
  LogReturns = c(NA, diff(log(coredata(AdjClose))))
)

# Rename columns for clarity
colnames(WMT_df) <- c("Date", "AdjClose", "LogReturns")

# Load csv
WMT_df <- read.csv("Walmart_AdjPrice.csv")

# Convert Date column to Date type
WMT_df$Date <- as.Date(WMT_df$Date, format = "%Y-%m-%d")

# Calculate Bollinger Bands
rolling_mean <- rollmean(WMT_df$AdjClose, k = 20, fill = NA, align = "right")
rolling_sd <- rollapply(WMT_df$AdjClose, width = 20, FUN = sd, 
                        fill = NA, align = "right")
bollinger_upper <- rolling_mean + 2 * rolling_sd
bollinger_lower <- rolling_mean - 2 * rolling_sd

# Add Bollinger Bands to the data frame
WMT_df$BollingerMean <- rolling_mean
WMT_df$BollingerUpper <- bollinger_upper
WMT_df$BollingerLower <- bollinger_lower

write.csv(WMT_df,"Walmart_AdjPrice.csv", row.names=FALSE)
