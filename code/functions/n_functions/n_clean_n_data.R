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

clean_n_data <- function(input_file_name) {

# Read in SmartChem data.
# Note that all values are concentrations in ppm or mg/L.
n_data_clean <- read_delim(input_file_name, col_names = FALSE, delim = ";") %>%
  select(X1, X4, X7, X8) %>%
  rename("sample_no_full" = X1,
         "concentration_mg/L" = X4,
         "run_time" = X7,
         "type" = X8) %>%
  pivot_wider(names_from = "type", values_from = "concentration_mg/L") %>%
  # mutate(run_time = str_sub(run_time, end = 10)) %>%
  group_by(sample_no_full, run_time) %>%
  # summarise_all(list(~first(na.omit(.)))) %>%
  rename("nh3" = "WNHR",
         "no2_no3" = "WNO6") %>%
  mutate("sample_type" = case_when(
    grepl("^[0-9]{3}E", sample_no_full) == TRUE ~ "extract",
    grepl("^[0-9]{3}L", sample_no_full) == TRUE ~ "leachate",
    grepl("^Blank.*L", sample_no_full) == TRUE ~ "blank_leachate",
    grepl("^Blank.*E", sample_no_full) == TRUE ~ "blank_extract",
    grepl("^NO2", sample_no_full) == TRUE ~ "no2_standard",
    grepl("^NO3", sample_no_full) == TRUE ~ "no3_standard")) %>%
  ungroup() %>%
  arrange(sample_no_full)

# Replace all negative values with 0
n_data_clean <- replace(n_data_clean, n_data_clean < 0, 0)

# Separate sample ID number and replicate number
n_data_clean <- n_data_clean %>%
  mutate("sample_no" = case_when(
    grepl("^[0-9]", sample_no_full) == TRUE ~
      as.numeric(str_sub(sample_no_full, end = 3)))) %>%
  mutate("rep_no" = case_when(
    grepl("^[0-9]", sample_no_full) == TRUE ~
      as.numeric(str_sub(sample_no_full, start = -1)))) %>%
  select(-c(sample_no_full))

return(n_data_clean)
}
