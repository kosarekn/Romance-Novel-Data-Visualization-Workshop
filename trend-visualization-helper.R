###################################################################
#                         CREATE DATA FRAME
###################################################################

## Load a special R package that will help us format the data frame
library(tidyverse)

## set a seed so the data does not change between my run of this code
set.seed(1234)

## Set our time period 2020-2025
years <- 2020:2025

widget_data <- expand_grid(
  year = years,
  color = c("Yellow", "Blue", "Red", "Green")
) %>%
  mutate(
    ## Setting sales with different trends by color
    sales = case_when(
      color == "Yellow" ~ 500 - (year - 2020) * 30 + rnorm(n(), 0, 20),
      color == "Blue" ~ 200 + (year - 2020) * 40 + rnorm(n(), 0, 15),
      color == "Red" ~ 150 + (year - 2020) * 50 + rnorm(n(), 0, 15),
      color == "Green" ~ 100 + (year - 2020) * 60 + rnorm(n(), 0, 15)
    ),
    sales = pmax(sales, 0), ## Ensure no negative sales
    sales = round(sales)
  )

## View the data
head(widget_data)




###################################################################
#                       SINGLE TREND LINE
###################################################################

# Aggregate total sales per year
total_sales <- widget_data %>%
  group_by(year) %>%
  summarise(sales = sum(sales))

plot1 <- ggplot(total_sales, aes(x = year, y = sales)) +
  geom_line(size = 1.2, color = "steelblue") +
  geom_point(size = 3, color = "steelblue") +
  labs(
    title = "Total Widget Sales (2020-2025)",
    subtitle = "All Widgets",
    x = "Year",
    y = "Number of Sales"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"))

png("/Users/f002yt8/Documents/GitHub/Romance-Novel-Data-Visualization-Workshop/images/single-trend.png", width = 1700, height = 1500, res = 300)
print(plot1)
dev.off()



###################################################################
#                       MULTI TREND LINE
###################################################################



plot2 <- ggplot(widget_data, aes(x = year, y = sales, color = color, group = color)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(
    title = "Widget Sales by Color (2020-2025)",
    subtitle = "Individual Trends by Widget Color",
    x = "Year",
    y = "Number of Sales",
    color = "Widget Color"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold")) +
  scale_color_manual(values = c("Yellow" = "#FFD700", "Blue" = "#4169E1", 
                                "Red" = "#DC143C", "Green" = "#228B22"))

png("/Users/f002yt8/Documents/GitHub/Romance-Novel-Data-Visualization-Workshop/images/multi-trend.png", width = 1700, height = 1500, res = 300)
print(plot2)
dev.off()



