# This script organizes and analyzes results from the SmartChem measurements
# of nitrite, nitrate, and ammonia.

# Sarah Gao
# December 2, 2021
# hellosarahgao@gmail.com

library("ggplot2")
library("tidyverse")
library("dplyr")
library("stringr")
library("stringi")
library("fs")

# Set plot themes
source("code/functions/set_plot_themes.R")
set_theme()

# Get file list
file_list <- paste0("data/raw_data/SmartChem_N_extractions/2022_samples/",
                    list.files(
                      "data/raw_data/SmartChem_N_extractions/2022_samples/",
                        pattern = "*.Csv"))

# Source and run function to clean N data
source("code/functions/n_functions/n_clean_n_data.R")
# Read all csv files in the folder and create a compiled dataframe
n_data_clean <- clean_n_data(file_list) %>%
  select(-run_time)

# Create column of total leachate based on having added 25mL of CaCl2
# and total extract based on having added 40mL of KCl
total_leach_ext <- n_data_clean %>%
  mutate(leach_nh3_mg = 25 * nh3_leachate,
         leach_no2_no3_mg = 25 * no2_no3_leachate) %>%
  mutate(ext_nh3_mg = 40 * nh3_extract,
         ext_no2_no3_mg = 40 * no2_no3_extract) %>%
  select(-c(nh3_leachate, no2_no3_leachate, nh3_extract, no2_no3_extract))

# Bring in soil weights to determine total mg of leachate
data_sheets_files <- dir_ls(
  path = "data/raw_data/data_sheets/", recurse = 0) %>%
  # Filter out the redundant data sheet that has extra columns for
  # dried soil weights
  path_filter(glob = "*dried_wts.csv", invert = TRUE)
raw_data_sheets <- read_csv(data_sheets_files)
clean_data_sheets <- raw_data_sheets %>%
  select(sample_no, n_extraction_soil_mass,
         n_leachate_soil_mass_pre,water_leachate) %>%
  select(-water_leachate) %>%
  arrange(sample_no)

# Get list of jar assignments from 2022
all_treatments <- readr::read_csv(
  "output/2022/jar_assignments/master_list.csv")

# Join and calculate total extract and total leachate by soil weights
total_leach_ext_wt <- total_leach_ext %>%
  left_join(clean_data_sheets) %>%
  left_join(all_treatments) %>%
  # Normalize total extracts and leachates to each sample's fresh soil weight
  mutate(leach_nh3_per = leach_nh3_mg / n_leachate_soil_mass_pre,
         leach_no2_no3_per = leach_no2_no3_mg / n_leachate_soil_mass_pre,
         ext_nh3_per = ext_nh3_mg / n_extraction_soil_mass,
         ext_no2_no3_per = ext_no2_no3_mg / n_extraction_soil_mass)

# Calculate sample medians compiling tech reps across all N types
samp_sum <- total_leach_ext_wt %>%
  select(-c(rep_no, n_extraction_soil_mass, n_leachate_soil_mass_pre)) %>%
  group_by(sample_no, cc_treatment, drying_treatment, pre_post_wet) %>%
  summarise(across(everything(),
                   .f = list(median = median), na.rm = TRUE))
# Note that Sample 12 was the only one that didn't have its leachate analysed
# so replaced all NAs with 0's except for Sample #12
sample_12 <- samp_sum %>%
  filter(sample_no == "12")
samp_sum <- samp_sum %>%
  filter(sample_no != "12") %>%
  mutate_at(vars(-c(sample_no)), ~ replace(., is.na(.), 0)) %>%
  rbind(sample_12) %>%
  arrange(sample_no)

# Create ratio of leachate : extract column
samp_sum_ratio <- samp_sum %>%
  mutate(ratio_nh3 = leach_nh3_per_median / ext_nh3_per_median,
         ratio_no2no3  = leach_no2_no3_per_median / ext_no2_no3_per_median)

### PER G FRESH SOIL PER WEEK ###
# Plot extract and leachate by week next to each other
source("code/functions/n_functions/plot_n_type.R")

# NH3 for each week
wk_nh3_sub <- post_samp_sum %>%
  select(sample_no, cc_treatment, drying_treatment,
         leach_nh3_per_median, ext_nh3_per_median) %>%
  pivot_longer(cols = c(leach_nh3_per_median, ext_nh3_per_median),
               names_to = "samp_type",
               values_to = "nh3_per")
facet_labels_nh3 <- c("leach_nh3_per_median" = "Leachate",
                      "ext_nh3_per_median" = "Total Extractable N")
nh3_plots <- plot_n_data(wk_nh3_sub, "nh3_per", facet_labels_nh3)
# Calculate stats per week to see effect of cover crop on
# aqueous N
nh3_stats <- wk_nh3_sub %>%
  wk_stats(., "nh3_per")

# NO2-NO3 for each week
wk_no2no3_sub <- post_samp_sum %>%
  select(sample_no, cc_treatment, drying_treatment,
         leach_no2_no3_per_median, ext_no2_no3_per_median) %>%
  pivot_longer(cols = c(leach_no2_no3_per_median, ext_no2_no3_per_median),
               names_to = "samp_type",
               values_to = "no2_no3_per")
facet_labels_no2no3 <- c("leach_no2_no3_per_median" = "Leachate",
                         "ext_no2_no3_per_median" = "Total Extractable N")
no2_no3_plots <- plot_n_data(wk_no2no3_sub, "no2_no3_per", facet_labels_no2no3)
# Calculate stats per week to see effect of cover crop on
# aqueous N
no2_no3_stats <- wk_no2no3_sub %>%
  wk_stats(., "no2_no3_per")


# Plot and analyze by time
source("code/functions/n_functions/summarize_treat_n_data.R")
# Calculate weekly statistics
source("code/functions/n_functions/calculate_weekly_stats.R")

### LEACHATE:EXTRACT RATIOS ###
# Plot w cc vs no cc at each post-wetting point
# Subset for post-wet and initial sets only
post_ratio_sum <- samp_sum_ratio %>%
  filter(pre_post_wet == "post" |
           pre_post_wet == "initial")

# Ratio of L:E in NH3
ratio_nh3 <- sum_plot_n(post_ratio_sum, "ratio_nh3", "nh3")
ratio_nh3_wk <- wk_stats(post_ratio_sum, "ratio_nh3")

# Ratio of L:E in NO2-NO3
ratio_no2_no3 <- sum_plot_n(post_ratio_sum, "ratio_no2no3", "no2-no3")
ratio_no2_no3_wk <- wk_stats(post_ratio_sum, "ratio_no2no3")




### THESE NEED TO BE NORMALIZED TO DRY SOIL WEIGHT TO COMPARE ACROSS TIME ###
# Leachate NH3 per gram plot + medians
# Subset for post-wet and initial sets only
post_samp_sum <- samp_sum %>%
  filter(pre_post_wet == "post" |
           pre_post_wet == "initial")

leach_nh3 <- analyze_plot_n(post_samp_sum, "leach_nh3_per_median", "nh3")
leach_nh3_wk <- wk_stats(post_samp_sum, "leach_nh3_per_median")

# Leachate NO2-NO3 per gram
leach_no2no3 <- analyze_plot_n(post_samp_sum, "leach_no2_no3_per_median",
                                    "no2-no3")
leach_no2no3_wk <- wk_stats(post_samp_sum, "leach_no2_no3_per_median")

# Extract NH3 per gram
ext_nh3 <- analyze_plot_n(post_samp_sum, "ext_nh3_per_median", "nh3")
ext_nh3_wk <- wk_stats(post_samp_sum, "ext_nh3_per_median")

# Extract NO2-NO3 per gram
ext_no2no3 <- analyze_plot_n(post_samp_sum, "ext_no2_no3_per_median",
                                  "no2-no3")
ext_no2no3_wk <- wk_stats(post_samp_sum, "ext_no2_no3_per_median")


# Test correlation between amount of water in leachate and N concentrations
sample_stats_leach <- sample_stats_all_wts %>% filter(sample_no != 12)
cor(sample_stats_leach$mean_nh3_leachate, sample_stats_leach$water_leachate)
cor(sample_stats_leach$mean_no2_no3_leachate, sample_stats_leach$water_leachate)
# This shows that there is low correlation between how dry the soil is and
# the concentration of N that is leached out.

