
rm(list = ls()) # init

if (!require("gridExtra")) {
  install.packages("gridExtra")
  stopifnot(require("gridExtra"))
}

if (!require("ggplot2")) {
  install.packages("ggplot2")
  stopifnot(require("ggplot2"))
}

if (!require("zoo")) {
  install.packages("zoo")
  stopifnot(require("zoo"))
}

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

# Plot 1: Adjusted Close Prices with Bollinger Bands
price_plot <- ggplot(WMT_df, aes(x = Date)) +
  geom_line(aes(y = AdjClose, color = "Adjusted Close Prices"), 
            linewidth = 0.7, na.rm = TRUE) +
  geom_ribbon(aes(ymin = BollingerLower, ymax = BollingerUpper, 
                  fill = "Bollinger Bands (Area)"), 
              alpha = 0.4, na.rm = TRUE) +
  geom_line(aes(y = BollingerUpper, color = "Bollinger Bands (Bounds)"), 
            linetype = "solid", linewidth = 0.5, na.rm = TRUE) +
  geom_line(aes(y = BollingerLower, color = "Bollinger Bands (Bounds)"), 
            linetype = "solid", linewidth = 0.5, na.rm = TRUE) +
  geom_line(aes(y = BollingerMean, color = "Rolling Mean"), 
            linewidth = 0.7, na.rm = TRUE) +
  scale_color_manual(
    name = "Line Legend",
    values = c(
      "Adjusted Close Prices" = "#007dc6",
      "Bollinger Bands (Bounds)" = "lightblue",
      "Rolling Mean" = "#444444"
    )
  ) +
  scale_fill_manual(
    name = "Fill Legend",
    values = c("Bollinger Bands (Area)" = "#e7f0f7")
  ) +
  theme_light() +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    legend.position = c(0.9, 0.25),
    legend.background = element_rect(fill = "white", 
                                     color = "black", size = 0.5),
    legend.title = element_text(face = "bold"),
    legend.key = element_blank()
  )

# Save the plot
ggsave("~/Documents/Walmart_Project/AdjPrice.png", plot=price_plot, 
       width = 12, height = 6, dpi = 300)
