# Clean workspace
rm(list = objects())  # Removes all objects from the environment.

# Load necessary library
library(tidyverse)

# Prices ----
prices <- tibble::tibble(
  year = rep(2015:2024, each = 12),
  month = rep(c("January", "February", "March", "April", "May", "June", 
                "July", "August", "September", "October", "November", "December"), times = 10),
  wheat_price = c(6.15, 5.89, 5.70, 5.56, 5.33, 5.42, 5.23, 4.84, 4.72, 4.86, 4.86, 4.75,
            4.82, 4.61, 4.40, 4.46, 4.45, 4.20, 3.75, 3.68, 3.48, 3.68, 3.88, 3.90,
            4.01, 4.16, 4.37, 4.16, 4.05, 4.37, 4.77, 4.84, 4.65, 4.64, 4.72, 4.50,
            4.65, 4.92, 5.10, 5.28, 5.39, 5.19, 5.00, 5.31, 5.15, 5.22, 5.23, 5.28,
            5.28, 5.33, 5.19, 4.93, 4.78, 4.81, 4.52, 4.34, 4.26, 4.45, 4.39, 4.64,
            4.88, 4.88, 4.86, 4.85, 4.76, 4.57, 4.54, 4.54, 4.73, 4.98, 5.24, 5.46,
            5.48, 5.83, 5.86, 6.04, 6.46, 6.23, 6.26, 7.14, 7.75, 7.92, 8.52, 8.59,
            8.48, 9.16, 9.93, 10.20, 10.90, 9.61, 8.69, 8.55, 8.78, 9.18, 9.15, 8.97,
            8.81, 8.54, 8.35, 8.31, 8.07, 7.67, 7.61, 7.34, 7.06, 6.96, 6.52, 6.79,
            6.77, 6.34, 6.01, 5.90, 6.19, 5.86, 5.52, 5.23, 5.36, 5.47, 5.45, 5.49),
  soy_price = c(10.30, 9.91, 9.85, 9.69, 9.58, 9.58, 9.95, 9.71, 9.05, 8.81, 8.68, 8.76,
            8.71, 8.51, 8.56, 9.01, 9.76, 10.20, 10.20, 9.93, 9.41, 9.30, 9.47, 9.64,
            9.71, 9.86, 9.69, 9.33, 9.29, 9.10, 9.42, 9.24, 9.35, 9.18, 9.22, 9.30,
            9.30, 9.50, 9.81, 9.85, 9.84, 9.55, 9.08, 8.59, 8.78, 8.59, 8.36, 8.56,
            8.64, 8.52, 8.52, 8.28, 8.02, 8.31, 8.38, 8.22, 8.35, 8.60, 8.59, 8.70,
            8.84, 8.60, 8.47, 8.35, 8.28, 8.34, 8.50, 8.66, 9.24, 9.63, 10.30, 10.60,
            10.90, 12.70, 13.20, 13.90, 14.80, 14.50, 14.10, 13.70, 12.20, 11.90, 12.10, 12.50,
            12.90, 14.70, 15.40, 15.80, 16.10, 16.40, 15.50, 15.30, 14.20, 13.50, 14.00, 14.40,
            14.50, 15.10, 14.90, 14.90, 14.40, 14.20, 14.70, 14.10, 13.20, 12.70, 13.00, 13.10,
            12.80, 11.90, 11.80, 11.80, 11.90, 11.80, 11.30, 10.30, 10.20, 9.91, 9.84, 9.79)) |>
  mutate(date = as.Date(paste(year, month, "1"), format="%Y %B %d"),
         ratio = wheat_price / soy_price) |>
  mutate(wheat_USDton = round(wheat_price * 36.7437,2),
         soy_USDton = round(soy_price * 36.7437,2)) |>
  glimpse()

# Load necessary libraries
#library(ggplot2)
#library(ggside)
#library(dplyr)
#library(scales)  # For better date formatting

# Compute averages
wheat_avg <- mean(prices$wheat_USDton, na.rm = TRUE)
soy_avg <- mean(prices$soy_USDton, na.rm = TRUE)
ratio_avg <- mean(prices$ratio, na.rm = TRUE)

# Create the plot
ggplot(prices, aes(x = date)) +
  # Line plots for Wheat and Soybean prices
  geom_line(aes(y = wheat_USDton), color = "#FF5F05", size = 0.3) +
  geom_line(aes(y = soy_USDton), color = "#13294B", size = 0.3) +
  # Add horizontal dashed lines for average wheat and soy prices
  geom_hline(yintercept = wheat_avg, linetype = "dotted", color = "#FF5F05") +
  geom_hline(yintercept = soy_avg, linetype = "dotted", color = "#13294B") +
  # Display text for average wheat and soy prices
  annotate("text", x = min(prices$date), y = wheat_avg, family = 'Times New Roman',
           label = paste0("Avg Wheat: $", round(wheat_avg, 2)),
           hjust = 0, vjust = -1, color = "#FF5F05", size = 3) +
  annotate("text", x = min(prices$date), y = soy_avg, family = 'Times New Roman',
           label = paste0("Avg Soybean: $", round(soy_avg, 2)),
           hjust = 0, vjust = -1, color = "#13294B", size = 3) +
  # Scale settings for primary and secondary y-axes
  scale_y_continuous(name = bquote("Price (USD t"^"-1"*")"),
                     breaks = c(seq(0,600, by=100)),# limits = c(50,650),
                     sec.axis = sec_axis(~ . / 569.3216, name = "Price ratio (Wheat/Soybean)")) +
  # Density distribution of wheat/soy ratio on the right
  geom_ysidehistogram(aes(x = after_stat(density) * 569.3216, y = ratio * 569.3216),
    fill = '#707372', color = '#707372', bins = 30,
    alpha = 0.5, size = 0.3, position = "stack") +
  # Format x-axis to show **Jan & Jul** from 2015 to 2024
  scale_x_date(date_labels = "%b %Y", 
    breaks = seq(as.Date("2015-01-01"), as.Date("2024-07-01"), by = "6 months")) +
  # Labels and Themes
  labs(x = "Date",) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    ggside.panel.scale.y = 0.2,
    axis.text.x = element_text(angle = 45, hjust = 1),  # Fix x-axis readability
    ggside.panel.spacing.y = unit(1, "lines"),  # Space adjustment
    ggside.axis.text.x = element_blank(), ggside.axis.ticks.x = element_blank(),
    legend.title = element_text(size = 12, family = 'Times New Roman'),
    legend.text = element_text(size = 10, family = 'Times New Roman'),
    axis.text = element_text(size = 8, family = 'Times New Roman'),
    axis.title = element_text(size = 10, family = 'Times New Roman')
  )
ggsave('Figures/Figure3.tif', width = 7, height = 3, units = 'in', dpi = 300)
ggsave('Figures/Figure3.png', width = 7, height = 3, units = 'in', dpi = 300)

# Test weight ----
# Convert TW from lb/bu to g/L
TW_discount <- read.csv('Data/TWDiscountTables2024.csv', na.strings = '-99') |>
  mutate(TW = TW * 12.87198,  # Convert TW to g/L
         TW_group = cut(TW, 
                        breaks = seq(46 * 12.87198, 60.5 * 12.87198, by = 0.5 * 12.87198), 
                        labels = c(paste0(seq(46, 59.5, by = 0.5) * 12.87198, '-', seq(46.4, 59.9, by = 0.5) * 12.87198), "765.88281+"),
                        right = FALSE)) |>
  filter(!is.na(TW_group)) |>
  group_by(TW_group) |>
  summarise(across(starts_with("D2024"), ~ unique(.x), .names = "{.col}"),
            TW = min(TW)) |>
  relocate(TW, .before = TW_group) |>
  rowwise() %>%
  mutate(Average_Discount = mean(c_across(starts_with("D2024")), na.rm = TRUE),
         Average_Discount = round(Average_Discount,2)) |>
  pivot_longer(cols = 3:9, names_to = 'Source', values_to = 'Discount') |>
  arrange(desc(TW)) |>
  mutate(Discount = Discount *36.7437,
         Average_Discount = Average_Discount * 36.7437) |>
  glimpse()

# Convert test weight range for filtering regression models
TW_min <- 46 * 12.87198
TW_max <- 58 * 12.87198
TW_51 <- 51 * 12.87198
TW_53 <- 53 * 12.87198

# Filter data for each interval
data_46_58 <- TW_discount %>% filter(TW >= TW_min & TW <= TW_max)
data_51_58 <- TW_discount %>% filter(TW >= TW_51 & TW <= TW_max)
data_53_58 <- TW_discount %>% filter(TW >= TW_53 & TW <= TW_max)

# Fit regression models
model_46_58 <- lm(Discount ~ TW, data = data_46_58)
model_51_58 <- lm(Discount ~ TW, data = data_51_58)
model_53_58 <- lm(Discount ~ TW, data = data_53_58)

# Extract coefficients
coef_46_58 <- coef(model_46_58)
coef_51_58 <- coef(model_51_58)
coef_53_58 <- coef(model_53_58)

# Create regression equations as text
eq_46_58 <- paste0("Slope = ", round(coef_46_58[2], 2), " USD g\u207B\u00B9")
eq_51_58 <- paste0("Slope = ", round(coef_51_58[2], 2), " USD g\u207B\u00B9")
eq_53_58 <- paste0("Slope = ", round(coef_53_58[2], 2), " USD g\u207B\u00B9")

# Define a set of 7 distinct shapes and colors
custom_shapes <- c(0, 1, 2, 3, 4, 5, 8)  # 7 distinct shapes
custom_colors <- c("red", "blue4", "darkgreen", "darkviolet", "darkorange", "brown4", "deepskyblue3")  # 7 colors

# Plot with TW in g/L
ggplot(TW_discount, aes(x = TW, y = Discount)) +
  geom_point(aes(color = Source, shape = Source), size = 2, alpha = 0.5) +
  scale_x_reverse(
    name = bquote("Test weight (g L"^"-1"*")"),  # TW (g L⁻¹)
    breaks = round(seq(40, 60, by = 1)*12.87198, 0)) +
  
  scale_y_continuous(#name = 'Price discount (USD per ton)', 
                     name = bquote("Price discount (USD t"^"-1"*")"),
                     breaks = seq(0, -120, by = -30)) +
  scale_color_manual(name = "Source", values = custom_colors, labels = (1:7)) +  
  scale_shape_manual(name = "Source", values = custom_shapes, labels = (1:7)) +
  
  # Vertical dashed lines
  annotate("segment", x = TW_max, xend = TW_max, y = min(TW_discount$Discount, na.rm = TRUE), yend = 0, 
           linetype = "solid", color = "black", size = 0.2, alpha = 0.5) +
  annotate("segment", x = TW_min, xend = TW_min, y = -3.2*36.7437, yend = -1.84*36.7437, 
           linetype = "dashed", color = "black", size = 0.2, alpha = 0.5) +
  annotate("segment", x = TW_51, xend = TW_51, y = -2.7*36.7437, yend = -1.02*36.7437, 
           linetype = "dashed", color = "black", size = 0.2, alpha = 0.5) +
  annotate("segment", x = TW_53, xend = TW_53, y = -2.2*36.7437, yend = -0.69*36.7437, 
           linetype = "dashed", color = "black", size = 0.2, alpha = 0.5) +
  
  # Regression lines
  stat_smooth(data = data_46_58, method = "lm", se = FALSE, size = 0.4, linetype = 'dotted',
              geom = 'line', alpha = 0.5, color = 'black') +
  stat_smooth(data = data_51_58, method = "lm", se = FALSE, size = 0.4, linetype = 'longdash',
              geom = 'line', alpha = 0.5, color = 'black') +
  stat_smooth(data = data_53_58, method = "lm", se = FALSE, size = 0.4, linetype = 'solid',
              geom = 'line', alpha = 0.5, color = 'black') +
  
  # Regression equations
  annotate("text", x = TW_max * 0.97, y = -3.1*36.7437, label = eq_46_58, size = 3, family = 'Times New Roman') +
  annotate("text", x = TW_max * 0.97, y = -2.6*36.7437, label = eq_51_58, size = 3, family = 'Times New Roman') +
  annotate("text", x = TW_max * 0.97, y = -2.1*36.7437, label = eq_53_58, size = 3, family = 'Times New Roman') +
  
  # Double arrows for intervals
  annotate("segment", x = TW_min, xend = TW_max, y = -3.2*36.7437, yend = -3.2*36.7437,
           arrow = arrow(ends = "both", length = unit(0.1, "inches")), size = 0.4) +
  annotate("segment", x = TW_51, xend = TW_max, y = -2.7*36.7437, yend = -2.7*36.7437,
           arrow = arrow(ends = "both", length = unit(0.1, "inches")), size = 0.4) +
  annotate("segment", x = TW_53, xend = TW_max, y = -2.2*36.7437, yend = -2.2*36.7437,
           arrow = arrow(ends = "both", length = unit(0.1, "inches")), size = 0.4) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    legend.title = element_text(size = 12, family = 'Times New Roman'),
    legend.text = element_text(size = 10, family = 'Times New Roman'),
    axis.text = element_text(size = 8, family = 'Times New Roman'),
    axis.title = element_text(size = 10, family = 'Times New Roman'),
    legend.position = c(0.05, 0.4),
    legend.background = element_rect(fill = "transparent", color = "transparent"),
    legend.direction = "vertical"
  )
ggsave('Figures/Figure4.tif', width = 7, height = 3, units = 'in', dpi = 300)
ggsave('Figures/Figure4.png', width = 7, height = 3, units = 'in', dpi = 300)
