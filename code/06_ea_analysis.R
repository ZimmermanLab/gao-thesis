# Sarah Gao
# July 28, 2021
# This script takes in EA data and evaluates the accuracy
# and precision of the standards.

library("readr")
library("tidyr")
library("dplyr")
library("ggplot2")
library("scales")

# First, we will examine the SRM data to see if there was any drift / trend
# over the run and to determine if we need any correcting factors.

# Read in and clean up EA data
source("code/functions/clean_ea_data.R")
ea_results_clean <- clean_ea_data("data/raw_data/EA_CN/20210727/210727_Run.csv")

# Calculate means and RSDs for both nitrogen and carbon
# for all SRM samples
source("code/functions/calculate_srm_stats.R")
srm_stats <- calculate_srm_stats(ea_results_clean)

# Plot the SRM values
srm_n_plot <- ea_results_clean %>%
  filter(sample == "SRM") %>%
  ggplot(aes(x = pos, y = n_per)) +
  geom_point() +
  labs(title = "Standard Reference Material Nitrogen Content",
       x = "Position", y = "N percentage")

srm_c_plot <- ea_results_clean %>%
  filter(sample == "SRM") %>%
  ggplot(aes(x = pos, y = c_per)) +
  geom_point() +
  labs(title = "Standard Reference Material Carbon Content",
       x = "Position", y = "C percentage")

ggsave(filename = "output/ea_plots/srm_n_plot.png", srm_n_plot)
ggsave(filename = "output/ea_plots/srm_c_plot.png", srm_c_plot)

# Calculate means and RSDs for each sample
