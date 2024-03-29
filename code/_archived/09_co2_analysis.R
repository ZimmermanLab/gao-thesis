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
library("ggstatsplot")
library("grid")
library("rstatix")

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
set_theme("doc")

# List dates as days elapsed since rewetting
days_elapsed <- list("day0" = c("2022-02-22",
                                      "2022-03-08", "2022-03-15"),
                           "day1" = c("2022-02-23", "2022-03-02",
                                      "2022-03-09", "2022-03-16"),
                           "day2" = c("2022-02-24", "2022-03-03",
                                      "2022-03-10", "2022-03-17"),
                           "day3" = c("2022-02-25", "2022-03-11", "2022-03-18"),
                           "day4" = c("2022-02-26", "2022-03-12", "2022-03-19"))
# Dates of that first week of drying only
initial_dates <- list("dates" = c("2022-02-15", "2022-02-16", "2022-02-17",
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

# Save out samp_medians for correlation tests
write_csv(samp_medians, "data/cleaned_data/LICOR/samp_medians.csv")

#### CO2 TRIALS ####

# Find medians per sample per day
co2_tests_medians <- samp_medians  %>%
  filter(pre_post_wet == "post_co2")

# Plot by time
facet_names <- as_labeller(c("one_wk" = "One Week",
                             "two_wk" = "Two Weeks",
                             "four_wk" = "Four Weeks"))

# No cc subset
co2_tests_nocc <- co2_tests_medians %>%
  filter(drying_treatment != "initial_dry") %>%
  filter(cc_treatment == "no_cc")

# No cc plot
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

# With cc subset
co2_tests_wcc <- co2_tests_medians %>%
  filter(drying_treatment != "initial_dry") %>%
  filter(cc_treatment == "w_cc")

# With cc plot
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

#### EFFECT OF DRYING ON RESPIRATION ####
# initial_dry set only!
initial_dry_all <- samp_medians %>%
  filter(drying_treatment == "initial_dry") %>%
  mutate(day_elapsed = case_when(date == "2022-02-15" ~ "1",
                                 date == "2022-02-16" ~ "2",
                                 date == "2022-02-17" ~ "3",
                                 date == "2022-02-18" ~ "4",
                                 date == "2022-02-19" ~ "5"))
# Set facet names
facet_cc_names <- as_labeller(c("no_cc" = "No Cover Crop",
                                "w_cc" = "With Cover Crop"))

# No cc
init_dry_nocc <- initial_dry_all %>%
  filter(cc_treatment == "no_cc")
# Calculate medians + IQRs per day
init_dry_nocc_stats <- initial_dry_all %>%
  arrange(date) %>%
  group_by(date) %>%
  summarize(median_co2 = median(median), iqr_co2 = IQR(median))
# Test overall significance of drying
initial_dry_nocc %>%
  kruskal.test(data = ., median ~ date)

# W cc
init_dry_wcc_stats <- initial_dry_all %>%
  filter(cc_treatment == "w_cc") %>%
  arrange(date) %>%
  group_by(date) %>%
  summarize(median_co2 = median(median), iqr_co2 = IQR(median))
# Test significance
initial_dry_all %>%
  filter(cc_treatment == "w_cc") %>%
  kruskal.test(data = ., median ~ date)

# Plot cc_treatment side by side
init_dry_all_plot <- initial_dry_all %>%
  ggplot(aes(x = day_elapsed,
             y = median)) +
  geom_boxplot(aes(fill = cc_treatment,
    color = cc_treatment)) +
  labs(x = "Day of Drying",
       y = "CO2 (ppm)",
       title = "CO2 in Drying Soils") +
  facet_wrap(~ cc_treatment,
             labeller = facet_cc_names) +
  theme(legend.position = "none") +
  scale_color_manual(limits = c("no_cc", "w_cc"),
                     values = c("#097CB2", "#195004")) +
  scale_fill_manual(limits = c("no_cc", "w_cc"),
                    values = c("#16B4FF", "#34980D")) +
  scale_y_continuous(labels = label_comma(),
                     trans = "log10") +
  theme(panel.spacing = unit(2, "lines"))
  # Add overall Kruskal-Wallis p-values
  # stat_compare_means(label.x = 3.5, label.y = 4,
  #                   family = "Helvetica",
  #                   size = 6)
ggsave(init_dry_all_plot, filename =
         "output/2022/co2/figures/co2_drying_allcc.png",
       width = 14, height = 8, units = "in")

# Plot no cc on its own with pairwise significance
init_dry_nocc_plot <- init_dry_nocc %>%
  ggplot(aes(x = day_elapsed,
             y = median,
             fill = cc_treatment,
             color = cc_treatment)) +
  geom_boxplot() +
  labs(x = "Day of Drying",
       y = "CO2 (ppm)",
       title = "CO2 in Drying Soils") +
  facet_wrap(~ cc_treatment,
             labeller = facet_cc_names) +
  theme(legend.position = "none") +
  scale_color_manual(limits = c("no_cc", "w_cc"),
                     values = c("#097CB2", "#195004")) +
  scale_fill_manual(limits = c("no_cc", "w_cc"),
                    values = c("#16B4FF", "#34980D")) +
  scale_y_continuous(labels = label_comma()) +
  theme(panel.spacing = unit(2, "lines"))
ggsave(init_dry_nocc_plot,
       filename = "output/2022/co2/figures/co2_drying_nocc.png",
       width = 8, height = 8, units = "in")

# Save plot
ggsave(init_dry_all_plot,
       filename = "output/2022/co2/figures/co2_drying_wk.png",
       width = 14, height = 8, units = "in")

median(init_dry_wcc_stats$median_co2)
IQR(init_dry_wcc_stats$median_co2)
median(init_dry_nocc_stats$median_co2)
IQR(init_dry_nocc_stats$median_co2)
