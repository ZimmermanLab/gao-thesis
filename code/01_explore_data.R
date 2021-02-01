# Sarah Gao
# January 31, 2021
# This script is used to explore days between rainfall data in order to find
# the best parameters to use for drying/rewetting experiment.

library("readr")
library("dplyr")

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
mean_days_since_rain <- days_since_rain %>%
  filter(days_since_rain > 0) %>%
  group_by(Year) %>%


# Make a plot showing max and average number of days per year
