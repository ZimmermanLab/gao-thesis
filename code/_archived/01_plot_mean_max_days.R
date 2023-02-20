# Sarah Gao
# January 31, 2021
# This script is used to explore days between rainfall data in order to find
# the best parameters to use for drying/rewetting experiment.

library("readr")
library("dplyr")
library("ggplot2")
library("scales")

# Read in daily rainfall csv
daily_rainfall_3m <- readr::read_csv("data/raw_data/sf_station_3m.csv")

# Add a column showing days since last rainfall
# Note: val controls for the rainfall threshold
daily_rainfall_3m$days_since_rain <- unlist(mapply(
  function(len, val) if (val != 0) rep(0, len) else 1:len,
  rle(daily_rainfall_3m$Precipitation)$lengths,
  rle(daily_rainfall_3m$Precipitation)$values
))

# Create dataframe with count since last rainfall and year only
days_since_rain <- daily_rainfall_3m %>%
  select(days_since_rain, Year) %>%
  mutate(days_elapsed = abs(days_since_rain - lag(days_since_rain, default = 0))) %>%
  filter(days_elapsed > 1) %>%
  select(days_elapsed, Year)

# Find averages and maximums per year
mean_max_per_yr <- days_since_rain %>%
  group_by(Year) %>%
  summarize(mean_days = mean(days_elapsed), max_days = max(days_elapsed))

# Make a plot showing max and average number of days per year
ggplot(mean_max_per_yr, aes(x = Year,
                            y = max_days)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, fullrange = TRUE) +
  labs(y = "Number of Days",
       title = "Mean and Max Days Between Rain Events By Year") +
  scale_x_continuous(breaks= pretty_breaks(nrow(mean_max_per_yr)))

