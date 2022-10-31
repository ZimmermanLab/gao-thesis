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
library("scales")

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
samples <- auc_all %>%
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

# Set plot theme
source("code/functions/set_plot_themes.R")
set_theme()

# List dates as days elapsed since rewetting
days_elapsed <- data.frame("day0" = c("2022-02-22", "2022-03-08", "2022-03-15"),
                           "day1" = c("2022-02-23", "2022-03-09", "2022-03-16"),
                           "day2" = c("2022-02-24", "2022-03-10", "2022-03-17"),
                           "day3" = c("2022-02-25", "2022-03-11", "2022-03-18"),
                           "day4" = c("2022-02-26", "2022-03-12", "2022-03-19"))
# Dates of that first week of drying only
initial_dates <- data.frame(dates = c("2022-02-15", "2022-02-16", "2022-02-17",
                                      "2022-02-18", "2022-02-19"))

# Map all samples to treatments
samp_medians <- samp_mapped %>%
  # Make sure to group by sampling date!
  group_by(sample_no, drying_treatment,
           date, cc_treatment, pre_post_wet) %>%
  summarize(median = median(auc_norm),
            iqr = IQR(auc_norm)) %>%
  # Account for the fact that I used the two_wk drying set as the set I
  # first measured prior to any rewetting during that first week
  # IE These measurements reflect CO2 levels during the first week of
  mutate(drying_treatment = case_when(
    date %in% initial_dates$dates ~ "initial_dry",
    !(date %in% initial_dates$dates) ~ drying_treatment)) %>%
  # Column for days elapsed since rewetting. Day 0 means the
  # CO2 measurement right before rewetting.
  mutate(day_elapsed = case_when(date %in% days_elapsed$day0 ~ "0",
                                 date %in% days_elapsed$day1 ~
                                   "1",
                                 date %in% days_elapsed$day2 ~
                                   "2",
                                 date %in% days_elapsed$day3 ~
                                   "3",
                                 date %in% days_elapsed$day4 ~
                                   "4"))

#### CO2 TRIALS ####

# Find medians per sample per day
co2_tests_medians <- samp_medians  %>%
  filter(pre_post_wet == "post_co2")

# Plot by time
facet_names <- as_labeller(c("one_wk" = "One Week",
                             "two_wk" = "Two Weeks",
                             "four_wk" = "Four Weeks"))
co2_tests_nocc <- co2_tests_medians %>%
  filter(drying_treatment != "initial_dry") %>%
  filter(cc_treatment == "no_cc")
# Medians of peaks at each week of drying
co2_tests_nocc %>% filter(day_elapsed == "1") %>%
  group_by(drying_treatment, date) %>%
  summarize(day_median = median(median), day_iqr = IQR(median))
kruskal.test(data = co2_tests_nocc, median ~ drying_treatment)

co2_tests_plot_nocc <- co2_tests_nocc %>%
  ggplot(aes(x = day_elapsed,
             y = median)) +
  geom_boxplot(fill = "#16B4FF", color = "#097CB2") +
  facet_grid(~ factor(drying_treatment,
                      levels = c("one_wk", "two_wk", "four_wk")),
             labeller = facet_names) +
  labs(x = "Days After Rewetting",
       y = "CO2 Concentration (ppm)",
       title = "CO2 Levels After Rewetting Without Cover Crop")
# Save plot
ggsave(co2_tests_plot_nocc,
       filename = "output/2022/co2/figures/co2_time_nocc.png",
       width = 12, height = 8, units = "in")
# Testing significance of secondary peaks
co2_tests_medians %>%
  filter(drying_treatment == "one_wk") %>%
  filter(cc_treatment == "no_cc") %>%
  filter(day_elapsed == "3" | day_elapsed == "4") %>%
  kruskal.test(data = ., median ~ day_elapsed)
co2_tests_medians %>%
  filter(drying_treatment == "two_wk") %>%
  filter(cc_treatment == "no_cc") %>%
  filter(day_elapsed == "3" | day_elapsed == "4") %>%
  kruskal.test(data = ., median ~ day_elapsed)
co2_tests_medians %>%
  filter(drying_treatment == "four_wk") %>%
  filter(cc_treatment == "no_cc") %>%
  filter(day_elapsed == "3" | day_elapsed == "2" |
           day_elapsed == "4") %>%
  kruskal.test(data = ., median ~ day_elapsed)

# Trials over time with cc
co2_tests_wcc <- co2_tests_medians %>%
  filter(drying_treatment != "initial_dry") %>%
  filter(cc_treatment == "w_cc")
co2_tests_plot_wcc <- co2_tests_wcc %>%
  ggplot(aes(x = day_elapsed,
             y = median)) +
  geom_boxplot(fill = "#34980D", color = "#195004") +
  facet_grid(~ factor(drying_treatment,
                      levels = c("one_wk", "two_wk", "four_wk")),
             labeller = facet_names) +
  scale_y_continuous(labels = label_comma()) +
  labs(x = "Days After Rewetting",
       y = "CO2 Concentration (ppm)",
       title = "CO2 Levels After Rewetting With Cover Crop")
# Save plot
ggsave(co2_tests_plot_wcc,
       filename = "output/2022/co2/figures/co2_time_wcc.png",
       width = 12, height = 8, units = "in")

#### PEAK RESPIRATION BY DRYING TIME ####

# No cc only
# Compile data from post CO2 trials and  all samples
all_no_cc <- samp_medians %>%
  filter(cc_treatment == "no_cc")



# Medians of peaks at each week of drying
co2_wcc_med <- co2_tests_wcc %>%
  group_by(drying_treatment, day_elapsed) %>%
  mutate(day_median = median(median)) %>%
  group_by(drying_treatment) %>%
  summarize(wk_peak = max(day_median), wk_iqr = IQR(day_median))
kruskal.test(data = co2_tests_wcc, median ~ drying_treatment)



treat_no_cc <- samp_medians
  filter(pre_post_wet == "post" |
           pre_post_wet == "post_co2") %>%
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
