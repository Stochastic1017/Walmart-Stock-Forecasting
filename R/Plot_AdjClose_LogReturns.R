
# Clean the environment
rm(list = ls()) 

# Load required libraries
if (!require("gridExtra")) install.packages("gridExtra", dependencies = TRUE)
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
if (!require("zoo")) install.packages("zoo", dependencies = TRUE)
if (!require("grid")) install.packages("grid", dependencies = TRUE)

library(gridExtra)
library(ggplot2)
library(zoo)
library(grid)

# Load the dataset
WMT_df <- read.csv("Walmart_AdjPrice.csv")
WMT_df$Date <- as.Date(WMT_df$Date, format = "%Y-%m-%d")

# Compute empirical statistics for Walmart's log returns
empirical_mean <- mean(WMT_df$LogReturns, na.rm = TRUE)
empirical_sd <- sd(WMT_df$LogReturns, na.rm = TRUE)
ci_lower <- empirical_mean - 1.96 * empirical_sd
ci_upper <- empirical_mean + 1.96 * empirical_sd

# Normal distribution for comparison
normal_x <- seq(min(WMT_df$LogReturns, na.rm = TRUE), 
                max(WMT_df$LogReturns, na.rm = TRUE), length.out = 100)
normal_y <- dnorm(normal_x, mean = empirical_mean, sd = empirical_sd)

# Plot 1: Adjusted Close Prices with Bollinger Bands
price_plot <- ggplot(WMT_df, aes(x = Date)) +
  geom_line(aes(y = AdjClose), linewidth = 0.7, na.rm = TRUE, color = "#007dc6") +
  geom_ribbon(aes(ymin = BollingerLower, ymax = BollingerUpper), 
              alpha = 0.4, fill = "#e7f0f7", na.rm = TRUE) +
  geom_line(aes(y = BollingerUpper), 
            linetype = "solid", linewidth = 0.5, na.rm = TRUE, color = "lightblue") +
  geom_line(aes(y = BollingerLower), 
            linetype = "solid", linewidth = 0.5, na.rm = TRUE, color = "lightblue") +
  geom_line(aes(y = BollingerMean), 
            linewidth = 0.7, na.rm = TRUE, color = "#444444") +
  theme_light() +
  labs(x = "Date", y = "Adjusted Closing Price") +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    legend.position = "none" # Remove legend
  )

# Plot 2: Time-Series of Log Returns
time_series_plot <- ggplot(WMT_df, aes(x = Date)) +
  geom_rect(aes(xmin = min(Date, na.rm = TRUE), 
                xmax = max(Date, na.rm = TRUE), 
                ymin = ci_lower, 
                ymax = ci_upper), alpha = 0.3, fill = "#e7f0f7") +
  geom_line(aes(y = LogReturns), 
            linewidth = 0.5, na.rm = TRUE, color = "#79b9e7") +
  geom_hline(yintercept = 0.0, 
             color = "#444", linetype = "dashed", linewidth = 0.7) +
  theme_light() +
  labs(x = "Date", y = "Log Adjusted Returns") +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  theme(
    legend.position = "none", # Remove legend
    axis.title = element_text(size = 12)
  )

# Plot 3: Rotated Histogram of Log Returns
rotated_histogram <- ggplot(WMT_df, aes(x = LogReturns)) +
  geom_rect(aes(xmin = ci_lower, xmax = ci_upper), 
            ymin = 0, ymax = Inf, alpha = 0.3, fill = "#e7f0f7") +
  geom_histogram(aes(y = after_stat(density)), 
                 binwidth = 0.0075, color = "white", alpha = 0.7, fill = "#79b9e7", na.rm = TRUE) +
  geom_vline(xintercept = 0.0,
             color = "#444", linetype = "dashed", linewidth = 0.7) +
  stat_density(geom = "line",
               color = "red", linewidth = 1, alpha = 0.8, na.rm = TRUE) +
  geom_line(data = data.frame(x = normal_x, y = normal_y), 
            aes(x = x, y = y), color = "purple", linewidth = 1) +
  coord_flip() +
  labs(x = "", y = "Density") +
  theme_light() +
  theme(
    legend.position = "none", # Remove legend
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank()
  )

# Combine bottom row
bottom_row <- arrangeGrob(grobs = list(time_series_plot, rotated_histogram), 
                          ncol = 2, widths = c(3, 1))
print(bottom_row)

# TableGrob (1 x 2) "arrange": 2 grobs
# z     cells    name           grob
# 1 1 (1-1,1-1) arrange gtable[layout]
# 2 2 (1-1,2-2) arrange gtable[layout]

# Arrange the final layout
final_layout <- grid.arrange(
  price_plot,       # Top row
  bottom_row,       # Bottom two plots
  nrow = 2,         # Two rows
  heights = c(1, 1) # Adjust row heights
)

print(final_layout)

# TableGrob (2 x 1) "arrange": 2 grobs
# z     cells    name            grob
# 1 1 (1-1,1-1) arrange  gtable[layout]
# 2 2 (2-2,1-1) arrange gtable[arrange]
