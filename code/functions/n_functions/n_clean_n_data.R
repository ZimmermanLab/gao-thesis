# This function cleans up the raw N data from the SmartChem.

# Sarah Gao
# December 2, 2021
# hellosarahgao@gmail.com

# Load libraries
library("readr")
library("dplyr")
library("tidyr")
library("stringi")

clean_n_data <- function(input_file_name) {

# Read in SmartChem data.
# Note that all values are concentrations in ppm or mg/L.
n_data_raw <- readr::read_csv(input_file_name)
n_data_raw <- n_data_raw %>%
  select(SampleID, WNH3, WNO1, WNO3) %>%
  rename("sample_no" = SampleID, "nh3" = WNH3, "no2" = WNO1, "no2_no3" = WNO3)

# Separate sample ID number and replicate number
n_data_clean <- n_data_raw %>%
  mutate(sample_no = stri_reverse(sample_no)) %>%
  separate(sample_no, into = c("rep_no", "sample_no"), sep = 1) %>%
  mutate(sample_no = stri_reverse(sample_no)) %>%
  relocate(rep_no, .after = sample_no) %>%
  filter(str_detect(sample_no, "^[0-9]")) %>%
  mutate(id_match = as.numeric(
    sample_no == lag(sample_no, 1))) %>% # A value of 1 means it matches
  replace(is.na(.), 0)

# Replace all negative values with 0
n_data_clean <- replace(n_data_clean, n_data_clean < 0, 0)

return(n_data_clean)
}
