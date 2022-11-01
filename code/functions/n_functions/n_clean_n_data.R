# This function cleans up the raw N data from the SmartChem.

# Sarah Gao
# December 2, 2021
# hellosarahgao@gmail.com

# Load libraries
library("readr")
library("dplyr")
library("tidyr")
library("stringi")
library("stringr")

clean_n_data <- function(input_files) {

# Read in SmartChem data.
# Note that all values are concentrations in ppm or mg/L.
n_data_clean <- read_delim(input_files, delim = ";", col_names = FALSE) %>%
  select(X1, X4, X7, X8) %>%
  rename("sample_no_full" = X1,
         "concentration_mg_l" = X4,
         "run_time" = X7,
         "type" = X8) %>%
  # Use only the date, not time, for run time
  mutate(run_time = str_sub(run_time, end = 10)) %>%
  # Filter out weird bad runs
  filter(concentration_mg_l != -9999) %>%
  # Filter out standards
  filter(sample_no_full != "NO2E6",
         sample_no_full != "NO3E6",
         sample_no_full != "NH3E",
         str_detect(sample_no_full, "Blank") == FALSE,
         str_detect(sample_no_full, "BLANK") == FALSE) %>%
  group_by(sample_no_full) %>%
  pivot_wider(names_from = "type", values_from = "concentration_mg_l") %>%
  ungroup(sample_no_full) %>%
  unite(c(WNH3, WNHR), col = "nh3", na.rm = TRUE, remove = FALSE) %>%
  select(-c(WNH3, WNHR)) %>%
  rename("no2_no3" = "WNO6") %>%
  # Add column to show sample type
  mutate("sample_type" = case_when(
    grepl("^[0-9]{3}E", sample_no_full) == TRUE ~ "extract",
    grepl("^[0-9]{3}L", sample_no_full) == TRUE ~ "leachate")) %>%
  # Separate out replicate number
  mutate("sample_no" = case_when(
    grepl("^[0-9]", sample_no_full) == TRUE ~
      as.numeric(str_sub(sample_no_full, end = 3)))) %>%
  mutate("rep_no" = case_when(
    grepl("^[0-9]", sample_no_full) == TRUE ~
      as.numeric(str_sub(sample_no_full, start = -1)))) %>%
  select(-c(sample_no_full)) %>%
  # Pivot wider to have columns for extract Ns and leachate Ns
  pivot_wider(names_from = sample_type,
              values_from = c(nh3, no2_no3)) %>%
  relocate(c(sample_no, rep_no))

# Converts numbers to numeric and replaces all negative values with 0
n_data_clean <- n_data_clean %>%
  mutate_at(c(1:2, 4:6), as.numeric)
n_data_clean[n_data_clean < 0] <- 0

return(n_data_clean)
}
