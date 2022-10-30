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

# Read in summary of standardized data
auc_all <- read_csv("output/2022/co2/auc_norm_date")

# Filter out standards
# By date standardization
samples <- auc_all_date %>%
  filter(str_detect(sample_no, "ppm") == FALSE)

# Determine outliers in sample reps
samp_outliers <- samples %>%
  group_by(sample_no, date) %>%
  mutate(median = median(auc_norm),
            iqr = IQR(auc_norm)) %>%
  mutate(outlier_flag = case_when(auc_norm > median + (1.5 * iqr) |
                                    auc_norm < median - (1.5 * iqr)
                                  ~ "moderate",
                                  auc_norm > median + (3 * iqr) |
                                    auc_norm < median - (3 * iqr)
                                  ~ "extreme")) %>%
  arrange(sample_no, date)

samp_outliers$date <- as.character(samp_outliers$date)

# Bring in all treatment assignments
all_treatments <- read_csv("output/2022/jar_assignments/master_list.csv")
# Map to samples
samp_outliers$sample_no <- as.double(samp_outliers$sample_no)
samp_outliers$date <- as.character(samp_outliers$date)
samp_mapped <- samp_outliers %>%
  left_join(all_treatments)

# Filter out outliers
samp_mapped_nomod <- samp_mapped %>%
  filter(is.na(outlier_flag))


# Set plot theme
source("code/functions/set_plot_themes.R")
set_theme()

# Map all samples to treatments
samp_medians <- samp_mapped %>%
  # Filters out the CO2 tests "post_co2"
  filter(pre_post_wet == "post") %>%
  # Make sure to group by sampling date!
  group_by(sample_no, drying_treatment, date) %>%
  summarize(median = median(auc_norm),
            iqr = IQR(auc_norm))


#### CO2 TRIALS ####
# Plot the CO2 tests
initial_dates <- c("2022-02-15", "2022-02-16", "2022-02-17", "2022-02-18",
                   "2022-02-19")
# Find medians per sample per day
co2_tests_medians <- samp_mapped  %>%
  filter(pre_post_wet == "post_co2") %>%
  # Account for the fact that I used the two_wk drying set as the set I
  # first measured prior to any rewetting during that first week
  # IE These measurements reflect CO2 levels during the first week of drying
  mutate(drying_treatment = case_when(date %in% initial_dates ~
                                        "initial_dry",
                                      !(date %in% initial_dates)
                                      ~ drying_treatment)) %>%
  # Column for days elapsed since rewetting. Day 0 means the
  # CO2 measurement right before rewetting.
  mutate(day_elapsed = case_when(date == "2022-02-22" |
                                   date == "2022-03-08" |
                                   date == "2022-03-15" ~
                                   "0",
                                 date == "2022-02-23" |
                                   date == "2022-03-09" |
                                   date == "2022-03-16" ~
                                   "1",
                                 date == "2022-02-24" |
                                   date == "2022-03-10" |
                                   date == "2022-03-17" ~
                                   "2",
                                 date == "2022-02-25" |
                                   date == "2022-03-11" |
                                   date == "2022-03-18" ~
                                   "3",
                                 date == "2022-02-26" |
                                   date == "2022-03-12" |
                                   date == "2022-03-19" ~
                                   "4")) %>%
  group_by(sample_no, drying_treatment, date, cc_treatment) %>%
  summarize(median = median(auc_norm),
            iqr = IQR(auc_norm))
# Plot by time
facet_names <- as_labeller(c("one_wk" = "One Week",
                             "two_wk" = "Two Weeks",
                             "four_wk" = "Four Weeks"))
co2_tests_plot_all <- co2_tests_medians %>%
  filter(drying_treatment != "initial_dry") %>%
  ggplot(aes(x = day_elapsed,
             y = median)) +
  geom_boxplot(fill = "#16B4FF", color = "#097CB2") +
  facet_grid(~ factor(drying_treatment,
                      levels = c("one_wk", "two_wk", "four_wk")),
             labeller = facet_names) +
  labs(x = "Days After Rewetting",
       y = "CO2 (ppm)",
       title = "CO2 Levels After Rewetting By Drying Duration")
# Save plot
ggsave(co2_tests_plot_all, filename = "output/2022/co2/co2_trials_time.png",
       width = 10, height = 8, units = "in")

# See if days elapsed has an effect on respiration
co2_elapsed_stats <- co2_tests_medians %>%
  filter(drying_treatment != "initial_dry") %>%
  kruskal.test(data = ., median ~ day_elapsed)





#### PEAK RESPIRATION BY DRYING TIME ####

# Find the peak for each sample between the days of sampling
samp_peaks <- samp_medians %>%
  group_by(sample_no, drying_treatment) %>%
  summarize(peak_co2 = max(median))
samp_peaks_plot <- samp_peaks %>%
  ggplot(aes(x = drying_treatment,
             y = peak_co2)) +
  geom_boxplot(fill = "#16B4FF", color = "#097CB2") +
  labs(x = "Drying Time",
       y = "Peak Co2 (ppm)",
       title = "Peak CO2 in Dried Soils") +
  scale_x_discrete(limits = c("one_wk", "two_wk", "four_wk"),
                   labels = c("One Week", "Two Weeks", "Four Weeks"))
# Save out plot
ggsave(samp_peaks_plot, filename = "output/2022/co2/co2_peaks_by_drying.png",
       width = 10, height = 8, units = "in")
# Calculate median + IQR
samp_peaks_med <- samp_peaks %>%
  group_by(drying_treatment) %>%
  summarize(med_co2 = median(peak_co2), iqr = IQR(peak_co2))
# Calculate stats
samp_peak_stats <- samp_peaks %>%
  kruskal.test(data = ., peak_co2 ~ drying_treatment)


# Try without outliers
samp2_medians_nomod <- samp2_mapped_nomod %>%
  # Filters out the CO2 tests "post_co2"
  filter(pre_post_wet == "post") %>%
  # Make sure to group by sampling date!
  group_by(sample_no, drying_treatment, date) %>%
  summarize(median = median(auc_norm),
            iqr = IQR(auc_norm))

samp2_peaks_nomod <- samp2_medians_nomod %>%
  group_by(sample_no, drying_treatment) %>%
  # Find the peak for each sample between the two days of sampling
  summarize(peak_co2 = max(median)) %>%
  group_by(drying_treatment) %>%
  summarize(med_co2 = median(peak_co2), iqr = IQR(peak_co2))

# Run stats
treat_all_stats <- samp_medians %>%
  kruskal.test(data = ., median ~ drying_treatment)
treat_all_plot <- samp_medians %>%
  ggplot(aes(x = drying_treatment,
             y = median)) +
  geom_boxplot(fill = "#16B4FF", color = "#097CB2") +
  scale_x_discrete(labels = c("One Week", "Two Weeks", "Four Weeks")) +
  labs(x = "Drying Time",
       y = "Peak CO2 (ppm)",
       title = paste("Peak CO2 After Rewetting in All Samples"))
# Save out plot
ggsave(treat_all_plot, filename = "output/2022/co2/co2_all_plot.png",
       width = 10, height = 8, units = "in")

# No cc only
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
  geom_boxplot(fill = "#16B4FF", color = "#097CB2") +
  scale_x_discrete(labels = c("One Week", "Two Weeks", "Four Weeks")) +
  labs(x = "Drying Time",
       y = "Peak CO2 (ppm)",
       title = paste("Peak CO2 After Rewetting Without Cover Crop"))
# Save out plot
ggsave(treat_no_cc_plot, filename = "output/2022/co2/no_cc_all_plot.png",
       width = 10, height = 8, units = "in")

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
  geom_boxplot(fill = "#34980D", color = "#195004") +
  scale_x_discrete(labels = c("One Week", "Two Weeks", "Four Weeks")) +
  labs(x = "Drying Time",
       y = "Peak CO2 (ppm)",
       title = paste("Peak CO2 After Rewetting With Cover Crop"))
# Save out plot
ggsave(treat_w_cc_plot, filename = "output/2022/co2/w_cc_all_plot.png",
       width = 10, height = 8, units = "in")

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
             fill = cc_treatment,
             color = cc_treatment)) +
  geom_boxplot() +
  scale_fill_manual(name = NULL, values = c("#16B4FF", "#34980D"),
                    labels = c("Without cover crop", "With cover crop")) +
  scale_color_manual(name = NULL, values = c("#097CB2", "#195004"),
                     labels = c("Without cover crop", "With cover crop")) +
  scale_x_discrete(labels = c("One Week", "Two Weeks", "Four Weeks")) +
  labs(x = "Drying Time",
       y = "Peak CO2 (ppm)",
       title = paste("Peak CO2 After Rewetting"))
# Save out plot
ggsave(cc_compare_all_plot, filename = "output/2022/co2/cc_compare_all_plot.png",
       width = 10, height = 8, units = "in")

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
