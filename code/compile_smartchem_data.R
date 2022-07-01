# Quick script to gather SmartChem data

# Sarah Gao
# June 29th 2022

library(dplyr)
library(readr)
library(tidyverse)
library("stringr")

# Get file list
file_list <- list.files("data/raw_data/SmartChem_N_extractions/2022_samples/")

# Read all csv files in the folder and create a list of dataframes
raw_data_each <- lapply(paste0("data/raw_data/SmartChem_N_extractions/2022_samples/",
                     list.files("data/raw_data/SmartChem_N_extractions/2022_samples/")),
              read_delim, col_names = FALSE, delim = ";")

# Combine each dataframe in the list into a single dataframe
raw_data_all <- do.call("rbind", raw_data_each) %>%
  select(X1, X4, X7, X8) %>%
  rename("sample_no" = X1,
         "concentration_mg/L" = X4,
         "run_time" = X7,
         "type" = X8
         ) %>%
  arrange(sample_no)

raw_data_all$`concentration_mg/L` <- as.numeric(raw_data_all$`concentration_mg/L`)

# Find samples that exceeded the 20.0 mg/L threshold
need_rerun <- raw_data_all %>%
  filter(`concentration_mg/L` > 19) %>%
  arrange(sample_no)

# Remove non-samples from list
samples_only <- raw_data_all %>%
  filter(grepl('^[0-9]', sample_no) == TRUE) %>%
  arrange(sample_no)

sample_list <- samples_only %>%
  select(sample_no)
sample_list$sample_no <- as.numeric(str_sub(samples_only$sample_no, end = 3))
unique(sample_list)
setdiff(1:133, sample_list$sample_no)
