# Sarah Gao
# January 31, 2021
# This script is used to explore days between rainfall data in order to find
# the best parameters to use for drying/rewetting experiment.

library("readr")

daily_rainfall_3m <- readr::read_csv("data/raw_data/sf_station_3m.csv")
