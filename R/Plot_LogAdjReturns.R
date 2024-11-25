
rm(list = ls()) # init

if (!require("gridExtra")) {
  install.packages("gridExtra")
  stopifnot(require("gridExtra"))
}

if (!require("ggplot2")) {
  install.packages("ggplot2")
  stopifnot(require("ggplot2"))
}

if (!require("grid")) {
  install.packages("grid")
  stopifnot(require("grid"))
}

# Load csv
WMT_df <- read.csv("Walmart_AdjPrice.csv")

# Convert Date column to Date type
WMT_df$Date <- as.Date(WMT_df$Date, format = "%Y-%m-%d")

# Compute empirical statistics for Walmart's log returns
empirical_mean <- mean(WMT_df$LogReturns, na.rm = TRUE)
empirical_var <- var(WMT_df$LogReturns, na.rm = TRUE)
empirical_sd <- sqrt(empirical_var)
ci_lower <- empirical_mean - 1.96 * empirical_sd
ci_upper <- empirical_mean + 1.96 * empirical_sd

# Normal distribution for comparison
normal_x <- seq(min(WMT_df$LogReturns, na.rm = TRUE), 
                max(WMT_df$LogReturns, na.rm = TRUE), length.out = 100)
normal_y <- dnorm(normal_x, mean = empirical_mean, sd = empirical_sd)

# Time-Series Plot of Log Returns
time_series_plot <- ggplot(WMT_df, aes(x = Date)) +
  geom_rect(aes(xmin = min(Date, na.rm = TRUE), xmax = max(Date, na.rm = TRUE), 
                ymin = ci_lower, ymax = ci_upper, 
                fill = "Confidence Interval"), 
            alpha = 0.3) +
  geom_line(aes(y = LogReturns, color = "Log Returns"), 
            linewidth = 0.5, na.rm = TRUE) +
  geom_hline(yintercept = 0.0, color = "#444", 
             linetype = "dashed", linewidth = 0.7) +
  scale_color_manual(name = "Legend", 
                     values = c("Log Returns" = "#79b9e7")) +
  scale_fill_manual(name = "Legend", 
                    values = c("Confidence Interval" = "#e7f0f7")) +
  theme_light() +
  labs(x = "Date", y = "Log Returns") +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  theme(
    legend.position = c(0.85, 0.85), # Legend inside the plot
    legend.background = element_rect(fill = "white", 
                                     color = "black", size = 0.5),
    legend.title = element_text(face = "bold")
  )

# Rotated Histogram of Log Returns
rotated_histogram <- ggplot(WMT_df, aes(x = LogReturns)) +
  geom_rect(aes(xmin = ci_lower, xmax = ci_upper, 
                fill = "Confidence Interval"), 
            ymin = 0, ymax = Inf, alpha = 0.3) +
  geom_histogram(aes(y = after_stat(density), fill = "Histogram"), 
                 binwidth = 0.002, 
                 color = "white", alpha = 0.7, na.rm = TRUE) +
  geom_vline(xintercept = 0.0, color = "#444", 
             linetype = "dashed", linewidth = 0.7) + # Fixed mapping warning
  stat_density(geom = "line", aes(color = "KDE"), linewidth = 1, 
               alpha = 0.8, na.rm = TRUE) +
  geom_line(data = data.frame(x = normal_x, y = normal_y), 
            aes(x = x, y = y, color = "Normal Distribution"), linewidth = 1) +
  coord_flip() +
  scale_color_manual(name = "Legend", 
                     values = c("KDE" = "#f47421", 
                                "Normal Distribution" = "#ffc120")) +
  scale_fill_manual(name = "Legend", 
                    values = c("Confidence Interval" = "#e7f0f7", 
                               "Histogram" = "#79b9e7")) +
  labs(x = "", y = "Density") +
  theme_light() +
  theme(
    legend.position = c(0.65, 0.2), # Legend inside the plot
    legend.background = element_rect(fill = "white", 
                                     color = "black", size = 0.5),
    legend.title = element_text(face = "bold"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank()
  )

# Combine the plots and add a master title
master_title <- textGrob("Walmart Adjusted Log Returns Analysis Plot", 
                         gp = gpar(fontsize = 16, fontface = "bold"))

merged_plot_with_title <- arrangeGrob(
  grobs = list(time_series_plot, rotated_histogram), # Combine the plots
  ncol = 2,
  widths = c(3, 1),
  top = master_title # Add the master title
)

# Save the combined plot with title
ggsave("~/Documents/Walmart_Project/LogAdjReturns.png", 
       plot = merged_plot_with_title, 
       width = 12, height = 6, dpi = 300)

