# This script analyses data from the qPCR assays.

# Sarah Gao
# July 27th, 2022
# hellosarahgao@gmail.com

library("tidyr")
library("dplyr")
library("readr")
library("stringi")
library("stringr")

# Compile list of file paths of Cq results from runs
files <- dir(
  "data/raw_data/qPCR/", recursive = TRUE, full.names = TRUE,
  pattern = "\\Quantification Cq Results.csv")

# Read in data from csv files
raw_data <- read_csv(files)

# Read in weird csv file from Run 1 on 7/19/2022
run1_clean <- read_csv("data/raw_data/qPCR/2022-07-19_Run1_fungal.csv") %>%
  filter(Fluor == "SYBR") %>%
  select(Sample, Cq)

# Clean up compiled data
clean_data <- raw_data %>%
  filter(Fluor == "SYBR") %>%
  select(Sample, Cq)

# Add in Run 1 and separate out replicate numbers
clean_data_all <- rbind(clean_data, run1_clean) %>%
  rename("sample_no_full" = Sample, "cq" = Cq) %>%
  mutate("sample_no" = as.numeric(str_sub(sample_no_full, end = 3))) %>%
  mutate("rep_no" = as.numeric(str_sub(sample_no_full, start = -1)))

# Filter out NA values and store them separately
na_only <- clean_data_all %>%
  filter(cq == "NaN") %>%
  filter(sample_no_full != "Blank")
clean_data_samples <- clean_data_all %>%
  filter(cq != "NaN") %>%
  filter(sample_no_full != "Blank") %>%
  select(sample_no, rep_no, cq) %>%
  arrange(sample_no)

# Bring in master list of jar assignments
all_treatments <- readr::read_csv("output/2022/jar_assignments/master_list.csv")

# Calculate means and SDs per sample
qpcr_data_stats <- clean_data_all %>%
  group_by(sample_no) %>%
  summarize(mean_cq = mean(cq),
            sd_cq = sd(cq)) %>%
  left_join(clean_data_samples)

# Calculate SD difference of each replicate to determine outliers
stats_clean <- qpcr_data_stats %>%
  mutate(outlier = (cq >= mean_cq + 0.3) | (cq <= mean_cq - 0.3))

qpcr_data_stats %>%
  group_by(pre_post_wet, cc_treatment, drying_treatment) %>%
  summarize(mean_all_cq = mean(mean_cq),
            sd_all_cq = sd(sd_cq))
