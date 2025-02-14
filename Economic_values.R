# Clean workspace
rm(list = objects())  # Removes all objects from the environment.

# Load necessary library
library(tidyverse)
#library(tibble)

# Prices ----

# Create a data frame for soybean prices
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
  glimpse()

# Load necessary libraries
#library(ggplot2)
#library(ggside)
#library(dplyr)
#library(scales)  # For better date formatting

# Compute averages
wheat_avg <- mean(prices$wheat_price, na.rm = TRUE)
soy_avg <- mean(prices$soy_price, na.rm = TRUE)
ratio_avg <- mean(prices$ratio, na.rm = TRUE)

# Create the plot
ggplot(prices, aes(x = date)) +
  # Line plots for Wheat and Soybean prices
  geom_line(aes(y = wheat_price), color = "#FF5F05", size = 0.3) +
  geom_line(aes(y = soy_price), color = "#13294B", size = 0.3) +
  # Add horizontal dashed lines for average wheat and soy prices
  geom_hline(yintercept = wheat_avg, linetype = "dotted", color = "#FF5F05") +
  geom_hline(yintercept = soy_avg, linetype = "dotted", color = "#13294B") +
  # Display text for average wheat and soy prices
  annotate("text", x = min(prices$date), y = wheat_avg, family = 'Times New Roman',
           label = paste0("Avg Wheat: $", round(wheat_avg, 2)),
           hjust = 0, vjust = -1, color = "#FF5F05", size = 4) +
  annotate("text", x = min(prices$date), y = soy_avg, family = 'Times New Roman',
           label = paste0("Avg Soybean: $", round(soy_avg, 2)),
           hjust = 0, vjust = -1, color = "#13294B", size = 4) +
  # Scale settings for primary and secondary y-axes
  scale_y_continuous(breaks = c(seq(4,16, by=2)),
                     sec.axis = sec_axis(~ . / 15.4944, name = "Wheat/Soybean price")) +
  # Density distribution of wheat/soy ratio on the right
  geom_ysidehistogram(aes(x = after_stat(density), y = ratio * 15.4944),
    fill = '#707372', color = '#707372',
    alpha = 0.5, size = 0.3, position = "stack") +
  # Density distribution of wheat/soy ratio on the right
  geom_ysidedensity(aes(x = after_stat(density), y = ratio * 15.4944),
                      fill = '#707372', color = '#707372',
                      alpha = 0.5, size = 0.3, position = "stack") +
  # Format x-axis to show **Jan & Jul** from 2015 to 2024
  scale_x_date(date_labels = "%b %Y", 
    breaks = seq(as.Date("2015-01-01"), as.Date("2024-07-01"), by = "6 months")) +
  # Labels and Themes
  labs(x = "Date",
    y = "Price (USD per bushel)",
    color = "Commodity") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    ggside.panel.scale.y = 0.2,
    axis.text.x = element_text(angle = 45, hjust = 1),  # Fix x-axis readability
    ggside.panel.spacing.y = unit(1, "lines"),  # Space adjustment
    legend.title = element_text(size = 12, family = 'Times New Roman'),
    legend.text = element_text(size = 10, family = 'Times New Roman'),
    axis.text = element_text(size = 8, family = 'Times New Roman'),
    axis.title = element_text(size = 10, family = 'Times New Roman')
  )
ggsave('Figures/Figure3.tif', width = 7, height = 3, units = 'in', dpi = 300)
ggsave('Figures/Figure3.png', width = 7, height = 3, units = 'in', dpi = 300)

# Test weight ----
TW_discount <- read.csv('Data/TWDiscountTables2024.csv', na.strings = '-99') |>
  mutate(TW_group = cut(TW, 
                        breaks = seq(46, 60.5, by = 0.5), 
                        labels = c(paste0(seq(46, 59.5, by = 0.5), '-', seq(46.4, 59.9, by = 0.5)), "58+"),
                        right = FALSE)) |>
  filter(!is.na(TW_group)) |>
  group_by(TW_group) |>
  summarise(across(starts_with("D2024"), ~ unique(.x), .names = "{.col}"),
            TW = min(TW)) |>
  relocate(TW,.before = TW_group) |>
  rowwise() %>%
  mutate(Average_Discount = mean(c_across(starts_with("D2024")), na.rm = TRUE),
         Average_Discount = round(Average_Discount,2)) |>
  pivot_longer(cols = 3:9, names_to = 'Source', values_to = 'Discount') |>
  arrange(desc(TW)) |>
#  mutate(Discount = str_replace(Discount,pattern = '-99', '-10')) |>
  glimpse()

# Define a set of 7 distinct shapes and colors
custom_shapes <- c(16, 17, 18, 19, 15, 8, 7)  # 7 distinct shapes
custom_colors <- c("red", "blue", "green", "purple", "orange", "brown", "pink")  # 7 colors

# Modify the ggplot call
ggplot(TW_discount, aes(x = TW, y = Discount)) +
  geom_line(aes(y = Average_Discount), size = 0.8, linetype = 'dashed', color = "black") +  
  geom_point(aes(color = Source, shape = Source), alpha = 0.7) +
  scale_x_reverse(name = 'TW (lb/bu)', breaks = seq(40, 60, by = 1)) +
  scale_y_continuous(name = 'Price discount (USD per bushel)', breaks = seq(0, -3.4, by = -0.5)) +
  scale_color_manual(name = "Source", values = custom_colors, labels = (1:7)) +  
  scale_shape_manual(name = "Source", values = custom_shapes, labels = (1:7)) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    legend.title = element_text(size = 12, family = 'Times New Roman'),
    legend.text = element_text(size = 10, family = 'Times New Roman'),
    axis.text = element_text(size = 8, family = 'Times New Roman'),
    axis.title = element_text(size = 10, family = 'Times New Roman'),
    legend.position = c(0.2, 0.2),  # Position legend inside bottom-left
    legend.background = element_rect(fill = "transparent", color = "transparent"),
    legend.direction = "horizontal" 
  ) +
  annotate("segment", x = 55.4, xend = 55.6, y = -2.86, yend = -2.86, linetype = "dashed", size = 0.8) +
  annotate("text", x = 55, y = -2.86, label = "Average", hjust = 0, size = 4, family = 'Times New Roman')

ggsave('Figures/Figure4.tif', width = 7, height = 3, units = 'in', dpi = 300)
ggsave('Figures/Figure4.png', width = 7, height = 3, units = 'in', dpi = 300)
