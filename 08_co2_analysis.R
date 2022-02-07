# This script takes in CO2 data from the LICOR gas analyzer and calculates
# CO2 concentrations

# Sarah Gao
# February 2, 2022
# hellosarahgao@gmail.com

# Load packages
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(MESS)

# Read in the data
hr1_01_raw <- read.csv(
  "data/raw_data/LICOR_CO2/20220111_tests/20220111_01_1hr_notime.txt",
  sep = "") %>%
  select(System_Time_.h.m.s., CO._.µmol_mol...,
         CO._Absorption, Flow_Rate_.L_min...) %>%
  rename(time = System_Time_.h.m.s., co2_ppm = CO._.µmol_mol...,
         co2_absorption = CO._Absorption, flow_rate = Flow_Rate_.L_min...)

# Replace negative CO2 values with 0
hr1_01_raw$co2_ppm[hr1_01_raw$co2_ppm < 0] <- 0

spline.d <- as.data.frame(spline(hr1_01_raw$time, hr1_01_raw$co2_ppm))
ggplot(hr1_01_raw, aes(x = time,
                       y = co2_ppm)) +
  geom_point() +
  geom_smooth(color = "blue", method="loess", se=TRUE, fullrange=FALSE, level=0.95)
require(MESS)
auc(hr1_01_raw$time, hr1_01_raw$co2_ppm, type = 'spline', from = "16:2:22", to = "16:2:26")
