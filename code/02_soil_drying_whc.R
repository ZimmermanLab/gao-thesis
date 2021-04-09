# Sarah Gao
# February 21, 2021
# This script is to analyse data from a soil drying test trial.

library("readr")
library("dplyr")
library("ggplot2")
library("scales")

# Read in data from drying trials
soil_drying <- readr::read_csv("data/raw_data/Trials_Results-Drying_Trial.csv")

# Create water holding capacity variables

tray_mass <- 370.13
tray_predry_soil_mass <- 765.10
predry_soil_mass <- tray_predry_soil_mass - tray_mass

# Post oven drying using soils from jar #1 + jar #4
tray_dry_soil_mass <- 725.65
dry_soil_mass <- tray_dry_soil_mass - tray_mass

# Rewetting with ~107.8g of soil (smaller soil sample)
prewet_soil_tray_mass <- 477.92
prewet_soil_mass <- prewet_soil_tray_mass - tray_mass

postwet_soil_tray_mass <- 523.40
postwet_soil_mass <- postwet_soil_tray_mass - tray_mass
water100_mass <- postwet_soil_mass - prewet_soil_mass
water100_as_dry_soil_per <- water100_mass / prewet_soil_mass

######

# Track water loss from jars #1 and #4 because you're dumb and did this
# totally wrong
jars_1_4 <- data.frame("days" = as.numeric(c(0,1,7)))

# Add in soil mass measures
jars_1_4$soil_mass <- as.numeric(c((soil_drying[1,7] + soil_drying[4,7]),
                        (soil_drying[1,8] + soil_drying[4,8]),
                        (soil_drying[1,9] + soil_drying[4,9])))

# Establish WHC as a percentage to dry mass variables
soil_dry_mass <- dry_soil_mass
soil_100_mass <- (water100_as_dry_soil_per * soil_dry_mass) + soil_dry_mass
water100_mass <- soil_100_mass - soil_dry_mass

# Add in water mass and water percentage measures
jars_1_4 <- jars_1_4 %>%
  mutate(
    water_mass = soil_mass - soil_dry_mass,
    water_per = water_mass / water100_mass
  )

# Plot water percentage over time
ggplot(jars_1_4, aes(x = days,
                     y = water_per)) +
  geom_line() +
  geom_point() +
  labs(x = "Days",
       y = "Percentage of WHC",
       title = "Water Content Over Time") +
  scale_x_continuous(breaks = pretty_breaks())
