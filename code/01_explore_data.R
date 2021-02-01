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

# Create dataframe with days since last rainfall and year only
days_since_rain_all <- daily_rainfall_3m %>%
  select(days_since_rain, Year)

# Setup empty list with column name
max_days_since_rain <- data.frame()

day_count <- 0
for (day in days_since_rain_all$days_since_rain) {
  day_count <- day_count + 1
  if (day == 0) {
    max_days_since_rain <- rbind(
      max_days_since_rain, days_since_rain_all$days_since_rain[day_count-1])
  }
}

# Label column and filter out the 0's
max_days_since_rain <- max_days_since_rain %>%
  rename("max_days_since_rain" = 1) %>%
  filter(max_days_since_rain > 0)

# Make a plot showing max and average number of days per year
