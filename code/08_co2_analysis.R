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
# Testing significance of secondary peaks
co2_tests_medians %>%
  filter(drying_treatment == "four_wk") %>%
  filter(cc_treatment == "w_cc") %>%
  filter(day_elapsed == "3" | day_elapsed == "4"|
           day_elapsed == "2") %>%
  kruskal.test(data = ., median ~ day_elapsed)

#### FIND PEAK RESPIRATION BY DRYING TIME ####

# No cc only
# Compile data from post CO2 trials and all samples
all_samp <- samp_medians %>%
  filter(drying_treatment != "initial_dry") %>%
  group_by(drying_treatment, sample_no, cc_treatment) %>%
  summarize(wk_peak = max(median))

all_no_cc <- all_samp %>%
  filter(cc_treatment == "no_cc")

wk_peaks_no_cc_plot <- all_no_cc %>%
  ggplot(aes(x = drying_treatment,
             y = wk_peak)) +
  geom_boxplot(fill = "#16B4FF", color = "#097CB2") +
  scale_x_discrete(limits = c("one_wk", "two_wk", "four_wk"),
                   labels = c("One Week", "Two Weeks", "Four Weeks")) +
  labs(x = "Drying Time",
       y = "Peak Co2 (ppm)",
       title = "Post Rewetting Peak CO2 in Samples Without Cover Crop")
ggsave(wk_peaks_no_cc_plot, filename = "output/2022/co2/figures/peak_no_cc.png",
       width = 10, height = 8, units = "in")

# Check stats of this difference
kruskal.test(data = all_no_cc, wk_peak ~ drying_treatment)

# Medians of peaks at each week of drying CO2 jars only
co2_nocc_med <- co2_tests_nocc %>%
  group_by(drying_treatment, day_elapsed) %>%
  mutate(day_median = median(median)) %>%
  group_by(drying_treatment) %>%
  summarize(wk_peak = max(day_median), wk_iqr = IQR(day_median))
kruskal.test(data = co2_tests_wcc, median ~ drying_treatment)
# Check medians of all samples
all_wk_peaks_nocc <- wk_peaks_no_cc %>%
  group_by(drying_treatment) %>%
  summarise(med_wk_peak = median(wk_peak), iqr_wk = IQR(wk_peak))


# With cc only
# Compile data from post CO2 trials and all samples
all_w_cc <- all_samp %>%
  filter(cc_treatment == "w_cc")

# Plot no cc and w cc in facet
wk_peaks_w_cc_plot <- all_w_cc %>%
  ggplot(aes(x = drying_treatment,
             y = wk_peak)) +
  geom_boxplot(fill = "#34980D", color = "#195004") +
  scale_x_discrete(limits = c("one_wk", "two_wk", "four_wk"),
                   labels = c("One Week", "Two Weeks", "Four Weeks")) +
  scale_y_continuous(labels = label_comma()) +
  labs(x = "Drying Time",
       y = "Peak Co2 (ppm)",
       title = "Post Rewetting Peak CO2 in Samples With Cover Crop")
ggsave(wk_peaks_w_cc_plot, filename = "output/2022/co2/figures/peak_w_cc.png",
       width = 10, height = 8, units = "in")

# Check stats of this difference
kruskal.test(data = wk_peaks_w_cc, wk_peak ~ drying_treatment)

# Medians of peaks at each week of drying for CO2 only
co2_wcc_med <- co2_tests_wcc %>%
  group_by(drying_treatment, day_elapsed) %>%
  mutate(day_median = median(median)) %>%
  group_by(drying_treatment) %>%
  summarize(wk_peak = max(day_median), wk_iqr = IQR(day_median))
kruskal.test(data = co2_tests_wcc, median ~ drying_treatment)

all_wk_peaks_wcc <- wk_peaks_w_cc %>%
  group_by(drying_treatment) %>%
  summarise(med_wk_peak = median(wk_peak), iqr_wk = IQR(wk_peak))

# Set cc facet names
facet_cc_names <- as_labeller(c("no_cc" = "Without Cover Crop",
                                "w_cc" = "With Cover Crop"))
# Plot no cc and w cc in facet
wk_peaks_all_plot <- all_samp %>%
  ggplot(aes(x = drying_treatment,
             y = wk_peak,
             fill = cc_treatment,
             color = cc_treatment)) +
  geom_boxplot() +
  facet_wrap(~ cc_treatment, scales = "free",
             labeller = facet_cc_names) +
  scale_x_discrete(limits = c("one_wk", "two_wk", "four_wk"),
                   labels = c("One Week", "Two Weeks", "Four Weeks")) +
  theme(legend.position = "none",
        strip.text = element_text(size = 12)) +
  scale_color_manual(limits = c("no_cc", "w_cc"),
                     values = c("#097CB2", "#195004")) +
  scale_fill_manual(limits = c("no_cc", "w_cc"),
                    values = c("#16B4FF", "#34980D")) +
  scale_y_continuous(labels = label_comma()) +
  theme(panel.spacing = unit(2, "lines")) +
  labs(x = "Drying Time",
       y = "Peak Co2 (ppm)",
       title = "Peak CO2 After Rewetting")
ggsave(wk_peaks_all_plot, filename = "output/2022/co2/figures/peak_all.png",
       width = 14, height = 8, units = "in")


#### EFFECT OF DRYING ON RESPIRATION ####
# initial_dry set only!
initial_dry_all <- samp_medians %>%
  filter(drying_treatment == "initial_dry")

# No cc
init_dry_nocc_stats <- initial_dry_all %>%
  filter(cc_treatment == "no_cc") %>%
  arrange(date) %>%
  group_by(date) %>%
  summarize(median_co2 = median(median), iqr_co2 = IQR(median))
# Test significance
initial_dry_all %>%
  filter(cc_treatment == "no_cc") %>%
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

# Plot side by side
init_dry_all_plot <- initial_dry_all %>%
  ggplot(aes(x = date,
             y = median,
             fill = cc_treatment,
             color = cc_treatment)) +
  geom_boxplot() +
  labs(x = "Day of Drying",
       y = "CO2 Concentration (ppm)",
       title = "CO2 Concentrations In Drying Soils") +
  facet_wrap(~ cc_treatment, scales = "free",
             labeller = facet_cc_names) +
  scale_x_discrete(labels = c("1", "2", "3", "4", "5")) +
  theme(legend.position = "none",
        strip.text = element_text(size = 12)) +
  scale_color_manual(limits = c("no_cc", "w_cc"),
                     values = c("#097CB2", "#195004")) +
  scale_fill_manual(limits = c("no_cc", "w_cc"),
                    values = c("#16B4FF", "#34980D")) +
  scale_y_continuous(labels = label_comma()) +
  theme(panel.spacing = unit(2, "lines"))
# Save plot
ggsave(init_dry_all_plot,
       filename = "output/2022/co2/figures/co2_drying_wk.png",
       width = 14, height = 8, units = "in")

median(init_dry_wcc_stats$median_co2)
IQR(init_dry_wcc_stats$median_co2)
median(init_dry_nocc_stats$median_co2)
IQR(init_dry_nocc_stats$median_co2)
