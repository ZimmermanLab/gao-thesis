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
library("ggsignif")

# Set plot themes
source("code/functions/set_plot_themes.R")
set_theme("doc")

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
  mutate(leach_nh3_ug = 25 * nh3_leachate,
         leach_no2no3_ug = 25 * no2_no3_leachate) %>%
  mutate(ext_nh3_ug = 40 * nh3_extract,
         ext_no2no3_ug = 40 * no2_no3_extract) %>%
  select(-c(nh3_leachate, no2_no3_leachate, nh3_extract, no2_no3_extract))

# Bring in soil weights to determine total ug of leachate
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
  mutate(leach_nh3_per = leach_nh3_ug / n_leachate_soil_mass_pre,
         leach_no2no3_per = leach_no2no3_ug / n_leachate_soil_mass_pre,
         ext_nh3_per = ext_nh3_ug / n_extraction_soil_mass,
         ext_no2no3_per = ext_no2no3_ug / n_extraction_soil_mass) %>%
  select(-c(leach_nh3_ug, leach_no2no3_ug, ext_nh3_ug, ext_no2no3_ug))

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
         ratio_no2no3  = leach_no2no3_per_median / ext_no2no3_per_median)
# Write out as csv
write_csv(samp_sum_ratio, "data/cleaned_data/SmartChem_N/samp_sum_ratio.csv")

# Plot and analyze by time
source("code/functions/n_functions/summarize_treat_n_data.R")
# Calculate weekly statistics
source("code/functions/n_functions/calculate_weekly_stats.R")

#### PAIRWISE STUFF ####
# Create Wilcoxon annotation
wilcox_annot <- data.frame(drying_treatment = "four_wk",
                           label = "Pairwise Wilcoxon \nRanked Sum Tests")

# Create function to not show NS in ggsignif plots
sigFunc <- function(x){
  if (x < 0.001){"***"}
  else if (x < 0.01){"**"}
  else if (x < 0.05){"*"}
  else if (x < 0.1){"."}
  else {NA}}

### CC DIFFERENCES BY WEEK ###
# Plot w cc vs no cc at each post-wetting point
# Note that these should be ideally normalized to dry soil weight instead of
# by fresh soil weight as they are currently

# Subset for post-wet and initial sets only
post_ratio_sum <- samp_sum_ratio %>%
  filter(pre_post_wet == "post" |
           pre_post_wet == "initial")

### NH3 OVER TIME ###

# Get NH3 stats + summary
nh3_wk <- wk_stats(post_ratio_sum, "nh3")
# Get NO2-NO3 stats + summary
no2no3_wk <- wk_stats(post_ratio_sum, "no2no3")

# Ratio of L:E in NH3
ratio_nh3_plot <- sum_plot_n(post_ratio_sum, "ratio_nh3", "nh3")$plot +
  scale_y_continuous(limits = c(0, 2.1)) +
  # Adds Wilcoxon pairwise comparisons
  geom_signif(comparisons = list(c("no_cc", "w_cc")), test = "wilcox.test",
              map_signif_level = sigFunc,
              family = "Helvetica", textsize = 6)
# Add test annotation
# geom_text(x = 1.5, y = 2, data = wilcox_annot, aes(label = label,
#                                                   family = "Helvetica",
#                                                   size = 16,
#                                                   lineheight = 0.9),
#          show.legend = F)
ggsave(ratio_nh3_plot, filename = "output/2022/n_plots/ratio_nh3.png",
       width = 14, height = 8, units = "in")

#  Extractable NH3 per gram across time
ext_nh3_plot <- sum_plot_n(post_samp_sum, "ext_nh3_per_median", "nh3")$plot +
  scale_y_continuous(limits = c(0, 40)) +
  # Adds Wilcoxon pairwise comparisons
  geom_signif(comparisons = list(c("no_cc", "w_cc")), test = "wilcox.test",
              map_signif_level = sigFunc,
              family = "Helvetica", textsize = 6)
ggsave(ext_nh3_plot, filename = "output/2022/n_plots/ext_nh3_per_median.png",
       width = 14, height = 8, units = "in")

# Leachate NH3 per gram plot across time + medians
leach_nh3_plot <- sum_plot_n(post_samp_sum, "leach_nh3_per_median", "nh3")$plot +
  scale_y_continuous(limits = c(0, 6.3)) +
  # Adds Wilcoxon pairwise comparisons
  geom_signif(comparisons = list(c("no_cc", "w_cc")), test = "wilcox.test",
              map_signif_level = sigFunc,
              family = "Helvetica", textsize = 6)
ggsave(leach_nh3_plot, filename = "output/2022/n_plots/leach_nh3_per_median.png",
       width = 14, height = 8, units = "in")

### NO2-NO3 ###

# Ratio of L:E in NO2-NO3
ratio_no2_no3_plot <- sum_plot_n(post_ratio_sum, "ratio_no2no3", "no2-no3")$plot +
  scale_y_continuous(limits = c(0, 1.8)) +
  # Adds Wilcoxon pairwise comparisons
  geom_signif(comparisons = list(c("no_cc", "w_cc")), test = "wilcox.test",
              map_signif_level = sigFunc,
              family = "Helvetica", textsize = 6) +
  # Add test annotation
  geom_text(x = 1.5, y = 1.7, data = wilcox_annot, aes(label = label,
                                                       family = "Helvetica",
                                                       size = 16,
                                                       lineheight = 0.9),
            show.legend = F)
ggsave(ratio_no2_no3_plot, filename = "output/2022/n_plots/ratio_no2no3.png",
       width = 14, height = 8, units = "in")

# Extract NO2-NO3 per gram across time
ext_no2no3_plot <- sum_plot_n(post_samp_sum, "ext_no2no3_per_median",
                              "no2-no3")$plot +
  scale_y_continuous(limits = c(0, 60)) +
  # Adds Wilcoxon pairwise comparisons
  geom_signif(comparisons = list(c("no_cc", "w_cc")), test = "wilcox.test",
              map_signif_level = sigFunc,
              family = "Helvetica", textsize = 6)
ggsave(ext_no2no3_plot, filename = "output/2022/n_plots/ext_no2no3_per_median.png",
       width = 14, height = 8, units = "in")

# Leachate NO2-NO3 per gram across time
leach_no2no3_plot <- sum_plot_n(post_samp_sum, "leach_no2no3_per_median",
                                "no2-no3")$plot +
  scale_y_continuous(limits = c(0, 42)) +
  # Adds Wilcoxon pairwise comparisons
  geom_signif(comparisons = list(c("no_cc", "w_cc")), test = "wilcox.test",
              map_signif_level = sigFunc,
              family = "Helvetica", textsize = 6)
ggsave(leach_no2no3_plot,
       filename = "output/2022/n_plots/leach_no2no3_per_median.png",
       width = 14, height = 8, units = "in")
