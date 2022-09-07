# This script takes in EA data and evaluates the accuracy
# and precision of the standards.

# Sarah Gao
# July 28, 2021
# hellosarahgao@gmail.com

library("readr")
library("tidyr")
library("dplyr")
library("ggplot2")
library("scales")
library("readxl")

############

# SRMs

# First, we will examine the SRM data to see if there was any drift / trend
# over the run and to determine if we need any correcting factors.

# Compile list of file paths of EA data
files <- list.files(path = "data/raw_data/EA_CN/2022",
                    recursive = TRUE, full.names = TRUE,
                    pattern = "\\w+_Run\\d_\\d{2}.(xls|XLS)")

# Read in and clean up EA data
source("code/functions/ea_functions/clean_ea_data.R")
ea_results_clean <- clean_ea_data(files) %>%
  filter(!(sample_no == "SRM" & n_mg > 0.8)) %>%
# High SRM N values from 6/7/22 probably means should be rerun
  filter(date != "06/07/2022")

# Calculate means and RSDs for both nitrogen and carbon
# for all SRM samples
# From Calla: RSDs for both N and C should be 5-10%
source("code/functions/ea_functions/ea_calculate_srm_stats.R")
srm_stats <- calculate_srm_stats(ea_results_clean)

############

# SRM PLOTS

# Plot the SRM values
srm_n_plot <- ea_results_clean %>%
  filter(sample_no == "SRM") %>%
  ggplot(aes(x = pos, y = n_per)) +
  geom_point() +
  labs(title = "Standard Reference Material Nitrogen Content",
       x = "Position", y = "N percentage")

srm_c_plot <- ea_results_clean %>%
  filter(sample_no == "SRM") %>%
  ggplot(aes(x = pos, y = c_per)) +
  geom_point() +
  labs(title = "Standard Reference Material Carbon Content",
       x = "Position", y = "C percentage")

ggsave(filename = "output/ea_plots/srm_n_plot.png", srm_n_plot)
ggsave(filename = "output/ea_plots/srm_c_plot.png", srm_c_plot)

############

# SAMPLES

# Calculate means and RSDs for each sample
source("code/functions/ea_functions/ea_calculate_sample_stats.R")
sample_stats <- calculate_sample_stats(ea_results_clean)

# Pull out list of questionable samples that need to be rerun
# based on high RSDs
need_rerun <- sample_stats %>%
  left_join(ea_results_clean) %>%
  filter(flag == "yes")

# Clean up sample names
sample_stats$sample_no <- as.numeric(str_sub(
  sample_stats$sample_no, start = -3))

# Map samples and results to master list of treatments
all_treatments <- read_csv("output/2022/jar_assignments/master_list.csv") %>%
  transform(sample_no = as.numeric(sample_no))

mapped_results <- sample_stats %>%
  left_join(all_treatments)

# Calculate means of each drying x cc treatment group across replicates and
# calculate C:N ratios
c_n_results_means <- mapped_results %>%
  group_by(drying_treatment, cc_treatment, pre_post_wet) %>%
  summarise(mean_n = mean(mean_n), mean_c = mean(mean_c)) %>%
  mutate("c_n_ratio" = mean_c / mean_n)

# Create a plot comparing C:N ratios
c_n_plot <- c_n_results_means %>%
  filter(pre_post_wet != "no_soil") %>%
  filter(pre_post_wet != "all_dry") %>%
  group_by(pre_post_wet, cc_treatment, drying_treatment) %>%
  ggplot(aes(x = pre_post_wet,
             y = c_n_ratio,
             fill = cc_treatment)) +
  geom_col(position = position_dodge()) +
  facet_grid(. ~ factor(drying_treatment,
                        levels = c("initial", "one_wk", "two_wk",
                                   "four_wk", "all_dry"))) +
  coord_cartesian(ylim=c(5, 9))


#  scale_x_discrete(limits = c("all_dry", "cw", "pre", "post"),
#                   labels = c("All Dry", "Constant Water", "Pre Wetting",
#                              "Post Wetting"))

# Run statistical analysis on C:N, excluding cw and no_soil jars
ea_final_stats <- c_n_results_means %>%
  filter(pre_post_wet != "no_soil") %>%
  filter(pre_post_wet != "cw") %>%
  lm(data = ., c_n_ratio ~ pre_post_wet * cc_treatment) %>%
  anova()
