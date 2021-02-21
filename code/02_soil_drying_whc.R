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


