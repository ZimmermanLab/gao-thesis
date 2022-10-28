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
cn_ratio_clean <- clean_ea_data(files_ratio)

# Flag outliers
ratio_flags <- flag_outliers(cn_ratio_clean, "ratio")

# Join with percentage data
cn_clean_all <- ratio_flags %>%
  left_join(percent_flags)

# Save out ratio outliers separately
ratio_outliers_only <- cn_clean_all %>%
  filter(!(is.na(outlier_flags_ratio)))

# Map samples and results to master list of treatments for all outlier
# thresholds
all_treatments <- read_csv("output/2022/jar_assignments/master_list.csv")

mapped_all <- cn_clean_all %>%
  left_join(all_treatments)
mapped_no_ext <- cn_clean_all %>%
  filter(outlier_flags_ratio == "moderate" |
           is.na(outlier_flags_ratio) |
           outlier_flags_per == "moderate" |
           is.na(outlier_flags_per)) %>%
  left_join(all_treatments)
mapped_no_mod <- cn_clean_all %>%
  filter(is.na(outlier_flags_ratio) |
           is.na(outlier_flags_per)) %>%
  left_join(all_treatments)

# Examine the effects of cc on %N by comparing w_cc and no_cc across all
# samples
n_compare_cc_all <- mapped_all %>%
  select(cc_treatment, n_per, sample_no) %>%
  filter(!is.na(sample_no))

n_compare_cc_all %>% group_by(cc_treatment) %>%
  summarize(median = median(n_per),
            iqr = IQR(n_per))

n_compare_cc_stats_all <- n_compare_cc_all %>%
  kruskal.test(data = ., n_per ~ cc_treatment)

n_compare_cc_all %>% ggplot(aes(x = cc_treatment,
                          y = n_per)) +
  geom_boxplot()

compare_cc_no_mod <- mapped_no_mod %>%
  select(cc_treatment, n_per, sample_no) %>%
  filter(!is.na(sample_no))
compare_cc_stats_no_mod <- compare_cc_no_mod %>%
  kruskal.test(data = ., n_per ~ cc_treatment)
compare_cc_no_mod %>% ggplot(aes(x = cc_treatment,
                              y = n_per)) +
  geom_boxplot()


# Compile across treatment replicates
source("code/functions/ea_functions/summarize_treatments.R")
treatments_all <- summarize_treatments(mapped_all)
treatments_no_ext <- summarize_treatments(mapped_no_ext)
treatments_no_mod <- summarize_treatments(mapped_no_mod)

# Create plots comparing C:N ratios
source("code/functions/ea_functions/create_ratio_plots.R")
ratio_plot_all <- create_ratio_plot(treatments_all)
ratio_plot_no_ext <- create_ratio_plot(treatments_no_ext)
ratio_plot_no_mod <- create_ratio_plot(treatments_no_mod)

# Overall effect of cc
treatments_all %>% lm(mean_mean_cper ~ cc_treatment, .) %>%
  anova()
treatments_no_ext %>% lm(mean_mean_nper ~ cc_treatment, .) %>%
  anova()


# Create plots comparing C and N percentages
source("code/functions/ea_functions/create_percentage_plots.R")
nper_plot_all <- create_per_plot(treatments_all)[1]
cper_plot_all <- create_per_plot(treatments_all)[2]
nper_plot_no_ext <- create_per_plot(treatments_no_ext)[1]
cper_plot_no_ext <- create_per_plot(treatments_no_ext)[2]
nper_plot_no_mod <- create_per_plot(treatments_no_mod)[1]
cper_plot_no_mod <- create_per_plot(treatments_no_mod)[2]

#### 2. CONSTANT WATER ANALYSES
# Look at changes over time in constantly watered samples
source("code/functions/ea_functions/examine_constant_water_time.R")
ratio_cw_plot_all <- examine_cw_time(treatments_all, "ratio")[1]
ratio_cw_stats_all <- examine_cw_time(treatments_all, "ratio")[2]
nper_cw_plot_all <- examine_cw_time(treatments_all, "nper")[1]
nper_cw_stats_all <- examine_cw_time(treatments_all, "nper")[2]
cper_cw_plot_all <- examine_cw_time(treatments_all, "cper")[1]
cper_cw_stats_all <- examine_cw_time(treatments_all, "cper")[2]

ratio_cw_plot_no_ext <- examine_cw_time(treatments_no_ext, "ratio")[1]
ratio_cw_stats_no_ext <- examine_cw_time(treatments_no_ext, "ratio")[2]
nper_cw_plot_no_ext <- examine_cw_time(treatments_no_ext, "nper")[1]
nper_cw_stats_no_ext <- examine_cw_time(treatments_no_ext, "nper")[2]
cper_cw_plot_no_ext <- examine_cw_time(treatments_no_ext, "cper")[1]
cper_cw_stats_no_ext <- examine_cw_time(treatments_no_ext, "cper")[2]
# Note that there is no difference in C:N ratio data between the extreme and
# moderate outliers subsetted datasets
nper_cw_plot_no_mod <- examine_cw_time(treatments_no_mod, "nper")[1]
nper_cw_stats_no_mod <- examine_cw_time(treatments_no_mod, "nper")[2]
cper_cw_plot_no_mod <- examine_cw_time(treatments_no_mod, "cper")[1]
cper_cw_stats_no_mod <- examine_cw_time(treatments_no_mod, "cper")[2]



# Look at effect of rewetting and cc by comparing changes at one,
# two, and four weeks between pre and post wet samples
source("code/functions/ea_functions/compare_rewetting.R")
ratio_rewet_plot_all <- compare_rewetting(treatments_all, "ratio")[1]
ratio_rewet_stats_all <- compare_rewetting(treatments_all, "ratio")[2]
nper_rewet_plot_all <- compare_rewetting(treatments_all, "nper")[1]
nper_rewet_stats_all <- compare_rewetting(treatments_all, "nper")[2]
cper_rewet_plot_all <- compare_rewetting(treatments_all, "cper")[1]
cper_rewet_stats_all <- compare_rewetting(treatments_all, "cper")[2]
# Note that there is no difference between the extreme and
# moderate outliers subsetted datasets
ratio_rewet_plot_no_ext <- compare_rewetting(treatments_no_ext, "ratio")[1]
ratio_rewet_stats_no_ext <- compare_rewetting(treatments_no_ext, "ratio")[2]
nper_rewet_plot_all <- compare_rewetting(treatments_no_ext, "nper")[1]
nper_rewet_stats_all <- compare_rewetting(treatments_no_ext, "nper")[2]
cper_rewet_plot_all <- compare_rewetting(treatments_no_ext, "cper")[1]
cper_rewet_stats_all <- compare_rewetting(treatments_no_ext, "cper")[2]

# Moderate significance of rewetting on w/cc %C:
cper_rewet_w_cc <- treatments_all %>%
  filter(pre_post_wet != "no_soil") %>%
  filter(pre_post_wet == "pre" |
           pre_post_wet == "post") %>%
  filter(drying_treatment == "four_wk" |
           drying_treatment == "one_wk" |
           drying_treatment == "two_wk" ) %>%
    filter(cc_treatment == "w_cc") %>%
    group_by(drying_treatment)
cper_rewet_w_cc %>% lm(data = ., mean_mean_cper ~ pre_post_wet) %>%
  anova()

# Look at effect of drying over time by looking at only dried samples
source("code/functions/ea_functions/compare_drying.R")
ratio_drying_plot_all <- compare_drying(treatments_all, "ratio")[1]
ratio_drying_stats_all <- compare_drying(treatments_all, "ratio")[2]
nper_drying_plot_all <- compare_drying(treatments_all, "nper")[1]
nper_drying_stats_all <- compare_drying(treatments_all, "nper")[2]
cper_drying_plot_all <- compare_drying(treatments_all, "cper")[1]
cper_drying_stats_all <- compare_drying(treatments_all, "cper")[2]
# Note that there is no difference between the extreme and
# moderate outliers subsetted datasets
ratio_drying_plot__no_ext <- compare_drying(treatments_no_ext, "ratio")[1]
ratio_drying_stats_no_ext <- compare_drying(treatments_no_ext, "ratio")[2]
nper_drying_plot__no_ext <- compare_drying(treatments_no_ext, "nper")[1]
nper_drying_stats_no_ext <- compare_drying(treatments_no_ext, "nper")[2]
cper_drying_plot__no_ext <- compare_drying(treatments_no_ext, "cper")[1]
cper_drying_stats_no_ext <- compare_drying(treatments_no_ext, "cper")[2]

