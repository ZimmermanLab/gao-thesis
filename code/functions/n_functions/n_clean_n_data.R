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
# Note that all concentration values are in mg/L.
n_data_clean <- read_delim(input_files, delim = ";", col_names = FALSE,
                           id = "run_or_plate_id") %>%
  select(X1, X4, X7, X8, run_or_plate_id) %>%
  mutate(units = "mg_per_l",
         measurement_type = "smartchem",
         subtype = NA) %>%
  rename("sample_id_full" = X1,
         "value" = X4,
         "analyzed_date" = X7,
         "subsubtype" = X8) %>%
  # Clean up run number
  mutate(run_or_plate_id = str_extract(run_or_plate_id, "Run\\d+")) %>%
  # Separate out sample_id number
  mutate("sample_id" = case_when(
    grepl("^B", sample_id_full) ~ sample_id_full,
    grepl("NO3E6", sample_id_full) ~
      "no3_std",
    grepl("NO2E6", sample_id_full) ~
      "no2_std",
    grepl("NH3E", sample_id_full) ~
      "nh3_std",
    grepl("^[0-9]", sample_id_full) ~
      str_sub(sample_id_full, end = 3))) %>%
  # Separate out replicate number
  mutate("tech_rep_number" = case_when(
    grepl("^[0-9]", sample_id_full) ~
      as.numeric(str_sub(sample_id_full, start = -1)),
    grepl("^B", sample_id_full) ~
      as.numeric(str_sub(sample_id_full, start = -1)),
    .default = NA)) %>%
  # Add column for sample type
  mutate(sample_type = "core") %>%
  # Add column to show standard, sample, or blank
  mutate(standard_sample_blank = case_when(
    grepl("^B", sample_id_full) ~ "blank",
    grepl("^[0-9]", sample_id_full) ~ "sample",
    .default = "standard")) %>%
  # Add column to show subsubtypes (i.e. extract vs leachate)
  mutate("subtype" = case_when(
    grepl("^[0-9]{3}E", sample_id_full) ~ "extract",
    grepl("^[0-9]{3}L", sample_id_full) ~ "leachate",
    grepl("E[0-9-]+$", sample_id_full) & !grepl("NO", sample_id_full)
    ~ "extract",
    grepl("L[0-9-]+$", sample_id_full) & !grepl("NO", sample_id_full)
    ~ "leachate")) %>%
  # Recode subtypes
  # Note that only Run #1 has "WNH3" instead of "WNHR" as it used a miscoded
  # NH3 protocol
  mutate(subsubtype = str_replace(subsubtype, "WNH3", "nh3")) %>%
  mutate(subsubtype = str_replace(subsubtype, "WNHR", "nh3")) %>%
  mutate(subsubtype = str_replace(subsubtype, "WNO6", "no2-no3")) %>%
  # Clean up "analyzed_date" column
  mutate(analyzed_date = str_sub(analyzed_date, end = 10)) %>%
  mutate(analyzed_date = strftime(as.Date(
           analyzed_date, "%m/%d/%Y"),"%Y%m%d")) %>%
  # Rearrange columns and drop "sample_id_full" column
  select(sample_id, sample_type, analyzed_date, run_or_plate_id,
         measurement_type, subtype, subsubtype, standard_sample_blank,
         tech_rep_number, value, units)

return(n_data_clean)
}
