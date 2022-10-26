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

# SRMs
# Examine the SRM data to see if there was any drift / trend
# over time and to determine if we need any correcting factors.

# Calculate means and RSDs for both nitrogen and carbon
# for all SRM samples
# From Calla: RSDs for both N and C should be 5-10%
source("code/functions/ea_functions/ea_calculate_srm_stats.R")
srm_stats <- calculate_srm_stats(cn_percent_clean)

############

# SAMPLES

# Compile C:N ratio data
files_ratio <- dir_ls(path = "data/raw_data/EA_CN/2022/",
                      recurse = 1,
                      regex = "\\w+_Run\\d_(repeat_)*(\\d{2}_)*ratio.(xls|XLS)")
cn_ratio_clean <- clean_ea_data(files_ratio)


# Calculate means and RSDs for each sample
# Samples that have an RSD > 10 are flagged
source("code/functions/ea_functions/ea_calculate_sample_stats.R")
sample_stats <- calculate_sample_stats(cn_ratio_clean)

# Pull out list of questionable samples that need to be rerun
# based on high RSDs (>= 10%)
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
            sd_mean_cn = sd(mean_c_n),
            rsd_mean_cn = sd_mean_cn / mean_mean_cn * 100)

# Create a plot comparing C:N ratios with all factors
c_n_plot_all <- compiled_results %>%
  filter(pre_post_wet != "no_soil") %>%
  group_by(pre_post_wet, cc_treatment, drying_treatment) %>%
  ggplot(aes(x = pre_post_wet)) +
  geom_col(aes(y = mean_mean_cn,
               fill = cc_treatment),
           position = position_dodge()) +
  geom_errorbar(aes(ymax = mean_mean_cn + sd_mean_cn,
                    ymin = mean_mean_cn - sd_mean_cn,
                    group = cc_treatment),
                position = position_dodge()) +
  coord_cartesian(ylim = c(16, 23)) +
  facet_grid(. ~ factor(drying_treatment,
                        levels = c("initial", "one_wk", "two_wk",
                                   "four_wk", "all_dry"))) +
  scale_x_discrete(limits = c("all_dry","initial", "cw", "pre", "post"),
                   labels = c("All-Dry", "Initial", "Constant",
                              "Pre-Wet", "Post-Wet"))

# Subset data to look at changes over time in constantly watered samples
constant_time <- compiled_results %>%
  filter(pre_post_wet != "no_soil") %>%
  filter(pre_post_wet == "initial" |
           pre_post_wet == "pre") %>%
  filter(drying_treatment == "initial" |
           drying_treatment == "one_wk" |
           drying_treatment == "two_wk" |
           drying_treatment == "four_wk")
constant_time_plot <- constant_time  %>%
  group_by(pre_post_wet, cc_treatment, drying_treatment) %>%
  ggplot(aes(x = pre_post_wet)) +
  geom_col(aes(y = mean_mean_cn,
               fill = cc_treatment),
           position = position_dodge()) +
  geom_errorbar(aes(ymax = mean_mean_cn + sd_mean_cn,
                    ymin = mean_mean_cn - sd_mean_cn,
                    group = cc_treatment),
                position = position_dodge()) +
  labs(title = "Constantly Watered Samples Over Time") +
  coord_cartesian(ylim = c(16, 23)) +
  facet_grid(. ~ factor(drying_treatment,
                        levels = c("initial", "one_wk", "two_wk",
                                   "four_wk", "all_dry")))

# Subset data to look at effect of rewetting by comparing changes
# at one, two, and four weeks between pre and post wet samples
pre_post_diff <- compiled_results %>%
  filter(pre_post_wet != "no_soil") %>%
  filter(pre_post_wet == "pre" |
           pre_post_wet == "post") %>%
  filter(drying_treatment == "one_wk" |
           drying_treatment == "two_wk" |
           drying_treatment == "four_wk")
pre_post_diff_plot <- pre_post_diff %>%
  group_by(pre_post_wet, cc_treatment, drying_treatment) %>%
  ggplot(aes(x = pre_post_wet)) +
  geom_col(aes(y = mean_mean_cn,
               fill = cc_treatment),
           position = position_dodge()) +
  geom_errorbar(aes(ymax = mean_mean_cn + sd_mean_cn,
                    ymin = mean_mean_cn - sd_mean_cn,
                    group = cc_treatment),
                position = position_dodge()) +
  labs(title = "Effect of Rewetting Across Time") +
  coord_cartesian(ylim = c(16, 23)) +
  facet_grid(. ~ factor(drying_treatment,
                        levels = c("one_wk", "two_wk",
                                   "four_wk", "all_dry"))) +
  scale_x_discrete(limits = c("pre", "post"),
                   labels = c("Pre-Wet", "Post-Wet"))

# Subset data to look at effect of drying over time by looking at
# only dried samples
drying_diff <- compiled_results %>%
  filter(pre_post_wet != "no_soil") %>%
  filter(pre_post_wet == "initial" |
           pre_post_wet == "pre" |
           pre_post_wet == "all_dry") %>%
  filter(drying_treatment == "one_wk" |
           drying_treatment == "two_wk" |
           drying_treatment == "four_wk" |
           drying_treatment == "all_dry")
drying_diff_plot %>% drying_diff %>%
  group_by(pre_post_wet, cc_treatment, drying_treatment) %>%
  ggplot(aes(x = pre_post_wet)) +
  geom_col(aes(y = mean_mean_cn,
               fill = cc_treatment),
           position = position_dodge()) +
  geom_errorbar(aes(ymax = mean_mean_cn + sd_mean_cn,
                    ymin = mean_mean_cn - sd_mean_cn,
                    group = cc_treatment),
                position = position_dodge()) +
  labs(title = "Effect of Drying Over Time") +
  coord_cartesian(ylim = c(16, 23)) +
  facet_grid(. ~ factor(drying_treatment,
                        levels = c("one_wk", "two_wk",
                                   "four_wk", "all_dry"))) +
  scale_x_discrete(limits = c("pre", "post", "all_dry"),
                   labels = c("Pre-Wet", "Post-Wet", "All-Dry"))
