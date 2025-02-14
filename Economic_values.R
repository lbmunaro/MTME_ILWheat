# Load necessary library
library(tibble)

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
            12.80, 11.90, 11.80, 11.80, 11.90, 11.80, 11.30, 10.30, 10.20, 9.91, 9.84, 9.79),
) |>
  mutate(date = as.Date(paste(year, month, "1"), format="%Y %B %d"),
         ratio = soy_price / wheat_price,
         diff = soy_price-wheat_price)

# Create the plot
ggplot(prices, aes(x = date)) +
  geom_line(aes(y = wheat_price, color = "Wheat Price"), size = 1) +
  geom_line(aes(y = soy_price, color = "Soybean Price"), size = 1) +
  geom_line(aes(y = ratio * 5, color = "Soybean/Wheat Ratio"), size = 1, linetype = "dotted") + 
  scale_y_continuous(
    name = "Price ($)",
    sec.axis = sec_axis(~ . / 5, name = "Soybean/Wheat Ratio")
  ) +
#  scale_color_manual(values = c("Wheat Price" = "blue", "Soybean Price" = "green", "Soybean/Wheat Ratio" = "red")) +
  labs(title = "Wheat and Soybean Price Fluctuations (2015-2024)",
       x = "Year",
       color = "Legend") +
  theme_bw()

summary(prices)
    