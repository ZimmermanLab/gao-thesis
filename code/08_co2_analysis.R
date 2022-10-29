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


