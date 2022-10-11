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

# SRMs

# First, we will examine the SRM data to see if there was any drift / trend
# over the run and to determine if we need any correcting factors.

# Compile list of file paths of EA data
files_percent <- dir_ls(path = "data/raw_data/EA_CN/2022/",
                recurse = 1,
                regex = "\\w+_Run\\d_\\d{2}_percent.(xls|XLS)")
files_ratio <- dir_ls(path = "data/raw_data/EA_CN/2022/",
                      recurse = 1,
                      regex = "\\w+_Run\\d_\\d{2}_ratio.(xls|XLS)")

# Read in and clean up percentage EA data
source("code/functions/ea_functions/clean_ea_data.R")
cn_percent_clean <- clean_ea_data(files_percent)

# Calculate means and RSDs for both nitrogen and carbon
# for all SRM samples
# From Calla: RSDs for both N and C should be 5-10%
source("code/functions/ea_functions/ea_calculate_srm_stats.R")
srm_stats <- calculate_srm_stats(cn_percent_clean)

############

# SAMPLES

# Compile C:N ratio data
cn_ratio_clean <- clean_ea_data(files_ratio)

# Calculate means and RSDs for each sample
source("code/functions/ea_functions/ea_calculate_sample_stats.R")
sample_stats <- calculate_sample_stats(cn_ratio_clean)

# Pull out list of questionable samples that need to be rerun
# based on high RSDs
need_rerun <- sample_stats %>%
  left_join(cn_ratio_clean) %>%
  filter(flag == "yes")

# Filter out need rerun samples
sample_stats_no_rerun <- sample_stats %>%
  filter(!(sample_no %in% need_rerun$sample_no)) %>%
  select(-(flag))

# Clean up sample names
sample_stats_no_rerun$sample_no <- as.numeric(str_sub(
  sample_stats_no_rerun$sample_no, start = -3))

# Map samples and results to master list of treatments
all_treatments <- read_csv("output/2022/jar_assignments/master_list.csv") %>%
  transform(sample_no = as.numeric(sample_no))

mapped_results <- sample_stats_no_rerun %>%
  left_join(all_treatments)

# Compile across treatment replicates
compiled_results <- mapped_results %>%
  group_by(drying_treatment, cc_treatment, pre_post_wet) %>%
  summarise(mean_mean_cn = mean(mean_c_n),
            sd_sd_cn = sd(sd_c_n))

# Create a plot comparing C:N ratios
c_n_plot <- compiled_results %>%
  filter(pre_post_wet != "no_soil") %>%
  filter(pre_post_wet != "all_dry") %>%
  group_by(pre_post_wet, cc_treatment, drying_treatment) %>%
  ggplot(aes(x = pre_post_wet)) +
  geom_col(aes(y = mean_mean_cn,
               fill = cc_treatment),
           position = position_dodge()) +
  geom_errorbar(aes(ymax = mean_mean_cn + sd_sd_cn,
                    ymin = mean_mean_cn - sd_sd_cn,
                    group = cc_treatment),
                position = position_dodge()) +
  facet_grid(. ~ factor(drying_treatment,
                        levels = c("initial", "one_wk", "two_wk",
                                   "four_wk", "all_dry"))) +
  coord_cartesian(ylim=c(16, 21))


#  scale_x_discrete(limits = c("all_dry", "cw", "pre", "post"),
#                   labels = c("All Dry", "Constant Water", "Pre Wetting",
#                              "Post Wetting"))

# Run statistical analysis on C:N, excluding cw and no_soil jars
ea_final_stats <- mapped_results %>%
  mutate(c_n_ratio = mean_c / mean_n) %>%
  filter(pre_post_wet != "no_soil") %>%
  filter(pre_post_wet != "cw") %>%
  glm(data = ., c_n_ratio ~ drying_treatment * pre_post_wet * cc_treatment) %>%
  summary()

# Subset data to look at one thing across time
# or within a timeframe look at pre/post wet
# or a pre/post wet comparing cc treatment
# Have targeted q's: eg is there interaction b/w week and pre/post


mapped_results %>%
  group_by(pre_post_wet, cc_treatment, drying_treatment) %>%
  mutate(mean_mean_n = mean(mean_n),
         sd_mean_n = sd(mean_n)) %>%
  filter(pre_post_wet != "no_soil") %>%
  filter(pre_post_wet != "all_dry") %>%
  group_by(pre_post_wet, cc_treatment, drying_treatment) %>%
  ggplot(aes(x = pre_post_wet)) +
  geom_col(aes(y = mean_mean_n,
               fill = cc_treatment),
           position = position_dodge()) +
  facet_grid(. ~ factor(drying_treatment,
                        levels = c("initial", "one_wk", "two_wk",
                                   "four_wk", "all_dry"))) +
  geom_errorbar(aes(ymax = mean_mean_n + sd_mean_n,
                    ymin = mean_mean_n - sd_mean_n,
                    group = cc_treatment),
                position = position_dodge())

mapped_results %>%
  group_by(pre_post_wet, cc_treatment, drying_treatment) %>%
  mutate(mean_mean_c = mean(mean_c),
         sd_mean_c = sd(mean_c)) %>%
  filter(pre_post_wet != "no_soil") %>%
  filter(pre_post_wet != "all_dry") %>%
  group_by(pre_post_wet, cc_treatment, drying_treatment) %>%
  ggplot(aes(x = pre_post_wet)) +
  geom_col(aes(y = mean_mean_c,
               fill = cc_treatment),
           position = position_dodge()) +
  facet_grid(. ~ factor(drying_treatment,
                        levels = c("initial", "one_wk", "two_wk",
                                   "four_wk", "all_dry"))) +
  geom_errorbar(aes(ymax = mean_mean_c + sd_mean_c,
                    ymin = mean_mean_c - sd_mean_c,
                    group = cc_treatment),
                position = position_dodge())
