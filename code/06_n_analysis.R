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

# Plot w cc vs no cc at each post-wetting point
ratio_nh3_plot <- samp_mapped %>%
  filter(pre_post_wet == "post") %>%
  ggplot(aes(x = drying_treatment,
             y = ratio_nh3,
             fill = cc_treatment)) +
  geom_boxplot()
ratio_no2no3_plot <- samp_mapped %>%
  filter(pre_post_wet == "post") %>%
  ggplot(aes(x = drying_treatment,
             y = ratio_no2no3,
             fill = cc_treatment)) +
  geom_boxplot()

# Plot Leachate Ns as mg per g of fresh soil
source("code/functions/n_functions/analyze_plot_n_data.R")
leach_samp_sum <- samp_sum %>%
  filter(pre_post_wet == "post")
leach_nh3_plot <- analyze_plot_n(leach_samp_sum, "leach_nh3_per_median", "nh3")

leach_no2no3_plot <- analyze_plot_n(leach_samp_sum, "leach_no2_no3_per_median",
                                    "no2-no3")
ext_nh3_plot <- analyze_plot_n(leach_samp_sum, "ext_nh3_per_median", "nh3")

# Plot Extract Ns as mg per g of fresh soil
ext_nh3_plot <- samp_sum %>%
  filter(pre_post_wet == "post") %>%
  ggplot(aes(x = drying_treatment,
             y = ext_nh3_per_median,
             fill = cc_treatment)) +
  geom_boxplot()
ext_no2no3_plot <- samp_sum %>%
  filter(pre_post_wet == "post") %>%
  ggplot(aes(x = drying_treatment,
             y = ext_no2_no3_per_median,
             fill = cc_treatment)) +
  geom_boxplot()






# Test correlation between amount of water in leachate and N concentrations
sample_stats_leach <- sample_stats_all_wts %>% filter(sample_no != 12)
cor(sample_stats_leach$mean_nh3_leachate, sample_stats_leach$water_leachate)
cor(sample_stats_leach$mean_no2_no3_leachate, sample_stats_leach$water_leachate)
# This shows that there is low correlation between how dry the soil is and
# the concentration of N that is leached out.

# Map data to assignments and find mean + SDs of all samples
source("code/functions/n_functions/treatment_stats.R")
treatment_stats_all <- summarize_treat_stats(samples_stats_all, all_treatments)
treatment_stats_no_ext <- summarize_treat_stats(
  samples_stats_no_ext, all_treatments)
treatment_stats_no_mod <- summarize_treat_stats(
  samples_stats_no_mod, all_treatments)

# Find ratio of leachate:total N

#############


# Create a plot comparing NH3 levels in extracts between groups with and
# without cover crops across all times
nh3_extracts_plot <- n_data_stats %>%
  filter(pre_post_wet != "no_soil",
         sample_type == "extract") %>%
  ggplot(aes(x = pre_post_wet,
             y = mean_mean_nh3,
             fill = cc_treatment)) +
  geom_col(position = position_dodge()) +
  facet_grid(~drying_treatment) +
  geom_errorbar(aes(ymax = mean_mean_nh3 + sd_mean_nh3,
                    ymin = mean_mean_nh3 - sd_mean_nh3),
                size = 0.25,
                width = 0.2,
                position = position_dodge(0.9)) +
  scale_x_discrete(limits = c("all_dry", "cw", "pre", "post"),
                   labels = c("All Dry", "Constant Water", "Pre Wetting",
                              "Post Wetting")) +
  # coord_cartesian(ylim=c(0, 8)) +
  labs(title = paste("NH3 in Samples With and Without Cover Crop\n",
                     " Residue in Soil Extracts"),
       x = "Water Treatments", y = "NH3 (ppm)",
       fill = "Cover Crop Treatment") +
  scale_fill_discrete(breaks = c("w_cc", "no_cc"),
                      labels = c("Without cover crop", "With cover crop")) +
  # These additions are for creating poster assets
  scale_fill_manual(values = c("no_cc" = "#00C2FF",
                               "w_cc" = "#3EA50D")) +
  theme(legend.position = "none")

# Create a plot comparing NH3 levels in leachates between groups with and
# without cover crops across all times
nh3_leach_plot <- n_data_stats %>%
  filter(pre_post_wet != "no_soil") %>%
  ggplot(aes(x = pre_post_wet,
             y = mean_mean_nh3,
             fill = cc_treatment)) +
  geom_col(position = position_dodge()) +
  facet_grid(sample_type ~ factor(drying_treatment, levels = c(
    "one_wk", "two_wk", "four_wk", "all_dry"))) +
  geom_errorbar(aes(ymax = mean_mean_nh3 + sd_mean_nh3,
                    ymin = mean_mean_nh3 - sd_mean_nh3),
                size = 0.25,
                width = 0.2,
                position = position_dodge(0.9)) +
  scale_x_discrete(limits = c("all_dry", "cw", "pre", "post"),
                   labels = c("All Dry", "Constant Water", "Pre Wetting",
                              "Post Wetting")) +
  coord_cartesian(ylim=c(0, 8)) +
  labs(title = paste("NH3 in Samples With and Without Cover Crop\n",
                     " Residue in Soil Leachates"),
       x = "Water Treatments", y = "NH3 (ppm)",
       fill = "Cover Crop Treatment") +
  scale_fill_discrete(breaks = c("w_cc", "no_cc"),
                      labels = c("Without cover crop", "With cover crop"))


# Run statistical analysis on extracts for NH3, excluding cw and no_soil jars
nh3_stats <- n_data_stats %>%
  filter(pre_post_wet != "no_soil",
         pre_post_wet != "cw",
         sample_type == "extract") %>%
  lm(data = ., mean_nh3 ~ pre_post_wet * cc_treatment) %>%
  anova()

# Create a plot comparing NO2 levels between groups with and
# without cover crops at 4 weeks
no2_compare_cc_plot <- n_data_stats %>%
  filter(pre_post_wet != "no soil") %>%
  ggplot(aes(x = pre_post_wet,
             y = mean_mean_no2,
             fill = cc_treatment)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(aes(ymax = mean_mean_no2 + sd_mean_no2,
                    ymin = mean_mean_no2 - sd_mean_no2),
                size = 0.25,
                width = 0.2,
                position = position_dodge(0.9)) +
  scale_x_discrete(limits = c("cw", "pre", "post"),
                   labels = c(
                     "Constant Water", "Pre Wetting", "Post Wetting")) +
  labs(title = paste("NO2- in Samples With and Without Cover Crop\n",
                     " Residue After 4 Weeks of Drying"),
       x = "Water Treatments", y = "NO2 (ppm)",
       fill = "Cover Crop Treatment") +
  scale_fill_discrete(breaks = c("w_cc", "no_cc"),
                      labels = c("Without cover crop", "With cover crop")) +
  # These additions are for creating poster assets
  scale_fill_manual(values = c("no_cc" = "#00C2FF",
                               "w_cc" = "#3EA50D")) +
  theme(legend.position = "none")


# Create a plot comparing NH3 loss as a ratio of NH3 concentration in
# leachate to total extractable N
nh3_ratio_plot <- n_data_stats %>%
  filter(pre_post_wet != "no_soil",
         sample_type == "extract") %>%
  group_by(pre_post_wet, cc_treatment, drying_treatment) %>%
  #IDK HOW TO DO THIS
  summarize(ratio = case_when())
  ggplot(aes(x = pre_post_wet,
             y = mean_mean_nh3,
             fill = cc_treatment)) +
  geom_col(position = position_dodge()) +
  facet_grid(~drying_treatment) +
  geom_errorbar(aes(ymax = mean_mean_nh3 + sd_mean_nh3,
                    ymin = mean_mean_nh3 - sd_mean_nh3),
                size = 0.25,
                width = 0.2,
                position = position_dodge(0.9)) +
  scale_x_discrete(limits = c("all_dry", "cw", "pre", "post"),
                   labels = c("All Dry", "Constant Water", "Pre Wetting",
                              "Post Wetting")) +
  # coord_cartesian(ylim=c(0, 8)) +
  labs(title = paste("NH3 in Samples With and Without Cover Crop\n",
                     " Residue in Soil Extracts"),
       x = "Water Treatments", y = "NH3 (ppm)",
       fill = "Cover Crop Treatment") +
  scale_fill_discrete(breaks = c("w_cc", "no_cc"),
                      labels = c("Without cover crop", "With cover crop"))
##########

# Run statistical analysis on NO2, excluding cw and no_soil jars
no2_stats <- n_data_stats %>%
  filter(pre_post_wet != "no_soil") %>%
  filter(pre_post_wet != "cw") %>%
  lm(data = ., mean_no2 ~ pre_post_wet * cc_treatment) %>%
  anova()

# Create a plot comparing NO3 levels between groups with and
# without cover crops at 4 weeks
no3_compare_cc_plot <- n_data_stats %>%
  filter(pre_post_wet != "no soil") %>%
  group_by(pre_post_wet, cc_treatment) %>%
  summarize(mean_mean_no3 = mean(mean_no3),
            sd_mean_no3 = sd(mean_no3)) %>%
  ggplot(aes(x = pre_post_wet,
             y = mean_mean_no3,
             fill = cc_treatment)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(aes(ymax = mean_mean_no3 + sd_mean_no3,
                    ymin = mean_mean_no3 - sd_mean_no3),
                size = 0.25,
                width = 0.2,
                position = position_dodge(0.9)) +
  scale_x_discrete(limits = c("cw", "pre", "post"),
                   labels = c(
                     "Constant Water", "Pre Wetting", "Post Wetting")) +
  labs(title = paste("NO3 in Samples With and Without Cover Crop\n",
                     " Residue After 4 Weeks of Drying"),
       x = "Water Treatments", y = "NO3 (ppm)",
       fill = "Cover Crop Treatment") +
  scale_fill_discrete(breaks = c("w_cc", "no_cc"),
                      labels = c("Without cover crop", "With cover crop")) +
  # These additions are for creating poster assets
  scale_fill_manual(values = c("no_cc" = "#00C2FF",
                               "w_cc" = "#3EA50D")) +
  theme(legend.position = "none")

# Run statistical analysis on NO3, excluding cw and no_soil jars
no3_stats <- n_data_stats %>%
  filter(pre_post_wet != "no_soil") %>%
  filter(pre_post_wet != "cw") %>%
  lm(data = ., mean_no3 ~ pre_post_wet * cc_treatment) %>%
  anova()
