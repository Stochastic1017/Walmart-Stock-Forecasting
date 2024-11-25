
rm(list = ls())

# install and load packages
if (!require("quantmod")) {
  install.packages("quantmod")
  stopifnot(require("quantmod"))
}

if (!require("fBasics")) {
  install.packages("fBasics")
  stopifnot(require("fBasics"))
}

if (!require("fGarch")) {
  install.packages("fGarch")
  stopifnot(require("fGarch"))
}

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

write.csv(WMT_df,"Walmart_AdjPrice.csv", row.names=FALSE)
