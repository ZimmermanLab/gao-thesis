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
library("fs")

############

# Compile list of file paths of percentage EA data
files_percent <- dir_ls(path = "data/raw_data/EA_CN/2022/",
                recurse = 1,
                regex = "\\w+_Run\\d_(repeat_)*(\\d{2}_)*percent.(xls|XLS)")

# Read in and clean up percentage EA data
# Filter out any weird sample runs that had 0
source("code/functions/ea_functions/clean_ea_data.R")
cn_percent_clean <- clean_ea_data(files_percent)

# Get outlier flags (>1.5x IQR = moderate, >3x IQR = extreme)
source("code/functions/ea_functions/flag_outliers.R")
percent_flags <- flag_outliers(cn_percent_clean, "percent")

# Save out outliers only
percent_outliers <- percent_flags %>%
  filter(!(is.na(outlier_flags_per)))

# SRMs
# Examine the SRM data to see if there was any drift / trend
# over time and to determine if we need any correcting factors.

# Calculate means and RSDs for both nitrogen and carbon
# for all SRM samples
# From Calla: RSDs for both N and C should be 5-10%
source("code/functions/ea_functions/ea_calculate_srm_stats.R")
srm_stats_all <- calculate_srm_stats(percent_flags)
srm_stats_no_ext <- percent_flags %>%
  filter(outlier_flags_per == "moderate" |
           is.na(outlier_flags_per)) %>%
  calculate_srm_stats()
srm_stats_no_mod <- percent_flags %>%
  filter(is.na(outlier_flags_per)) %>%
  calculate_srm_stats()

############

# SAMPLES

# Compile C:N ratio data
files_ratio <- dir_ls(path = "data/raw_data/EA_CN/2022/",
                      recurse = 1,
                      regex = "\\w+_Run\\d_(repeat_)*(\\d{2}_)*ratio.(xls|XLS)")
cn_ratio_clean <- clean_ea_data(files_ratio) %>%
  filter(!str_detect(sample_no, "SRM")) %>%
  mutate(sample_no = as.numeric(sample_no))

# Flag outliers
ratio_flags <- flag_outliers(cn_ratio_clean, "ratio")

# Join with percentage data
cn_clean_all <- percent_flags %>%
  filter(!str_detect(sample_no, "SRM")) %>%
  mutate(sample_no = as.numeric(sample_no)) %>%
  left_join(ratio_flags)

# Map samples and results to master list of treatments
all_treatments <- read_csv("output/2022/jar_assignments/master_list.csv")

mapped_all <- cn_clean_all %>%
  left_join(all_treatments)

# Examine the effects of cc on %N by comparing w_cc and no_cc when dried
# ie initial + pre-wet samples only
# Calculate medians per sample
n_dry_samp_sum_all <- mapped_all %>%
  filter(pre_post_wet == "pre" |
           pre_post_wet == "initial") %>%
  group_by(sample_no, cc_treatment, drying_treatment) %>%
  summarize(samp_median = median(n_per))
# Calculate medians per treatment level
n_dry_treat_sum_all <- n_dry_samp_sum_all %>%
  group_by(cc_treatment, drying_treatment) %>%
  summarize(treat_median = median(samp_median),
            treat_iqr = IQR(samp_median))

# Plot
facet_drying_labels <- as_labeller(c("one_wk" = "One Week",
                                     "two_wk" = "Two Weeks",
                                     "four_wk" = "Four Weeks",
                                     "initial" = "Initial"))
n_dry_plot_all <- n_dry_samp_sum_all %>%
  ggplot(aes(x = factor(cc_treatment, levels = c("no_cc", "w_cc")),
             y = samp_median,
             fill = cc_treatment,
             color = cc_treatment)) +
  geom_boxplot() +
  facet_grid(~ factor(drying_treatment,
                      levels = c("initial", "one_wk", "two_wk", "four_wk")),
             labeller = facet_drying_labels)  +
  scale_fill_manual(name = NULL, limits = c("no_cc", "w_cc"),
                    values = c("#16B4FF", "#34980D"),
                    labels = c("No Cover Crop", "With Cover Crop")) +
  scale_color_manual(name = NULL, limits = c("no_cc", "w_cc"),
                     values = c("#097CB2", "#195004"),
                     labels = c("No Cover Crop", "With Cover Crop")) +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom") +
  labs(y = "Percent Nitrogen",
       title = "Nitrogen Percentage in Drying Soils")
ggsave(n_dry_plot_all, filename = "output/2022/ea_plots/nper_cc_drying.png",
       width = 14, height = 8, units = "in")
