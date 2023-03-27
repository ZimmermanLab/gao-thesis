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
# Note that all concentration values are in ppm or mg/L.
n_data_clean <- read_delim(input_files, delim = ";", col_names = FALSE) %>%
  select(X1, X4, X6, X7, X8) %>%
  rename("sample_no_full" = X1,
         "concentration_mg_l" = X4,
         "od" = X6,
         "run_time" = X7,
         "type" = X8) %>%
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
  relocate(sample_no, rep_no) %>%
  # Only Run #1 has "WNH3" instead of "WNHR" as it used a miscoded NH3 protocol
  mutate(type = str_replace(type, "WNH3", "nh3")) %>%
  mutate(type = str_replace(type, "WNHR", "nh3")) %>%
  mutate(type = str_replace(type, "WNO6", "no2-no3"))


# Converts numbers to numeric
n_data_clean <- n_data_clean %>%
  mutate_at(c(1:4), as.numeric)

return(n_data_clean)
}
