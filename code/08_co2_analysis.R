# This script takes in CO2 data from the LICOR gas analyzer and calculates
# CO2 concentrations

# Sarah Gao
# February 2, 2022
# hellosarahgao@gmail.com

# Load packages
library("readr")
library("tidyr")
library("dplyr")
library("ggplot2")
library("stringr")

# Source function to calculate AUCs
source("code/functions/co2_functions/find_peaks.R")

# Create list of file paths of CO2 data files
files_list <- list.files(path = "data/cleaned_data/LICOR/", recursive = FALSE,
                         pattern = "*.txt")
files_list <- subset(files_list, str_detect(files_list, "extract") == FALSE)

auc_summary_all <- data.frame()

for(f in 1:length(files_list)) {
  auc_summary <- find_peaks(files_list[f])
  auc_summary_all <- rbind(auc_summary_all, auc_summary)
}
# Save this out because it takes a long time to generate
write_csv(auc_summary_all, "output/2022/co2/auc_summary_all.txt")

####### START HERE IF ALREADY SAVED OUT SUMMARY #######

# Read in summary
auc_summary_all <- read_csv("output/2022/co2/auc_summary_all.txt")

# Create separate list of standards
auc_stds <- auc_summary_all %>%
  filter(str_detect(sample, "[0-9]{3}_[0-9]{2}") == FALSE)

# Separate out replicate number from sample number
auc_samples <- auc_summary_all %>%
  # Select samples (non-stds) only
  filter(str_detect(sample, "[0-9]{3}_[0-9]{2}") == TRUE) %>%
  mutate(sample_no = str_sub(sample, end = 3)) %>%
  mutate(rep_no = str_sub(sample, start = -2)) %>%
  select(-sample) %>%
  relocate(sample_no, total_auc)

# Determine outliers in sample reps
samples_outliers <- auc_samples %>%
  group_by(sample_no, date) %>%
  mutate(median = median(total_auc),
            iqr = IQR(total_auc)) %>%
  mutate(outlier_flag = case_when(total_auc > median + (1.5 * iqr) |
                                    total_auc < median - (1.5 * iqr)
                                  ~ "moderate",
                                  total_auc > median + (3 * iqr) |
                                    total_auc < median - (3 * iqr)
                                  ~ "extreme")) %>%
  arrange(sample_no, date)

samples_outliers$sample_no <- as.double(samples_outliers$sample_no)

# Bring in all treatment assignments
all_treatments <- read_csv("output/2022/jar_assignments/master_list.csv")
# Map to samples
samples_mapped <- samples_outliers %>%
  left_join(all_treatments)

treatments_all <- samples_mapped
# Filter out outliers
treatments_no_mod <- samples_mapped %>%
  filter(is.na(outlier_flag))

# Get stats for post rewetting only to see effect of drying treatment on
# peak respiration
treat_no_cc <- treatments_all %>%
  filter(pre_post_wet == "post") %>%
  filter(cc_treatment == "no_cc") %>%
  group_by(sample_no, drying_treatment)
summarize(group_by(treat_no_cc, drying_treatment),
          median = median(total_auc), iqr = IQR(total_auc))
treat_no_cc_stats <- treat_no_cc %>%
  kruskal.test(data = ., total_auc ~ drying_treatment)
treat_no_cc_plot <- treat_no_cc %>%
  ggplot(aes(x = factor(drying_treatment,
                        level = c("one_wk", "two_wk", "four_wk")),
             y = total_auc)) +
  geom_boxplot()

treat_w_cc <- treatments_all %>%
  filter(pre_post_wet == "post") %>%
  filter(cc_treatment == "w_cc") %>%
  group_by(sample_no, drying_treatment)
summarize(group_by(treat_w_cc, drying_treatment),
          median = median(total_auc), iqr = IQR(total_auc))
treat_w_cc_stats <- treat_w_cc %>%
  kruskal.test(data = ., total_auc ~ drying_treatment)
treat_w_cc_plot <- treat_w_cc %>%
  ggplot(aes(x = factor(drying_treatment,
                        level = c("one_wk", "two_wk", "four_wk")),
             y = total_auc)) +
  geom_boxplot()

# Try again with outliers filtered out
treat_no_cc_nomod <- treatments_no_mod %>%
  filter(pre_post_wet == "post") %>%
  filter(cc_treatment == "no_cc") %>%
  group_by(sample_no, drying_treatment)
summarize(group_by(treat_no_cc_nomod, drying_treatment),
          median = median(total_auc), iqr = IQR(total_auc))
treat_no_cc_nomod_stats <- treat_no_cc_nomod %>%
  kruskal.test(data = ., total_auc ~ drying_treatment)
treat_no_cc_nomod_plot <- treat_no_cc_nomod %>%
  ggplot(aes(x = factor(drying_treatment,
                        level = c("one_wk", "two_wk", "four_wk")),
             y = total_auc)) +
  geom_boxplot()

treat_w_cc_nomod <- treatments_no_mod %>%
  filter(pre_post_wet == "post") %>%
  filter(cc_treatment == "w_cc") %>%
  group_by(sample_no, drying_treatment)
summarize(group_by(treat_w_cc_nomod, drying_treatment),
          median = median(total_auc), iqr = IQR(total_auc))
treat_w_cc_nomod_stats <- treat_w_cc_nomod %>%
  kruskal.test(data = ., total_auc ~ drying_treatment)
treat_w_cc_nomod_plot <- treat_w_cc_nomod %>%
  ggplot(aes(x = factor(drying_treatment,
                        level = c("one_wk", "two_wk", "four_wk")),
             y = total_auc)) +
  geom_boxplot()

# Compare peak C loss as respiration between with and without cc
cc_compare_all <- treatments_all %>%
  group_by(cc_treatment, drying_treatment)
cc_compare_all_stats <- cc_compare_all %>%
  kruskal.test(data = ., total_auc ~ cc_treatment)
cc_compare_all_plot <- cc_compare_all %>%
  ggplot(aes(x = factor(drying_treatment,
                        level = c("one_wk", "two_wk", "four_wk")),
             y = total_auc,
             fill = cc_treatment)) +
  geom_boxplot()

# Same but w/o outliers
cc_compare_nomod <- treatments_no_mod %>%
  group_by(cc_treatment, drying_treatment)
cc_compare_nomod_stats <- cc_compare_nomod %>%
  kruskal.test(data = ., total_auc ~ cc_treatment)
cc_compare_nomod_plot <- cc_compare_nomod %>%
  ggplot(aes(x = factor(drying_treatment,
                        level = c("one_wk", "two_wk", "four_wk")),
             y = total_auc,
             fill = cc_treatment)) +
  geom_boxplot()
