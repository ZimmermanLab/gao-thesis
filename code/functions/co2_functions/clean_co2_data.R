# This function cleans up the CO2 data and prepares it to be merged to the
# master dataframe.

# Sarah Gao
# January 13, 2024
# hellosarahgao@gmail.com

# Load libraries
library("dplyr")
library("tidyr")

clean_co2_data <- function(input_file) {
  co2_data_clean <- read_csv(input_file) %>%
    # Clean up and rename columns to fit master dataframe
    rename(analyzed_date = date,
           value = auc_norm,
           sample_id = sample_no) %>%
    mutate(analyzed_date = strftime(as.Date(
      analyzed_date, "%Y-%m-%d"),"%Y%m%d"),
      # Use same date for sampled_date as analyzed_date since for LICOR it's
      # at the same time
      sampled_date = analyzed_date) %>%
    mutate(units = "auc") %>%
    # Add columns for standards
    mutate(std_co2_amount = case_when(
      str_detect(sample_id, "ppm") ~ str_extract(sample_id, "^\\d+"))) %>%
    mutate(std_co2_units = case_when(
      is.na(std_co2_amount) == FALSE ~ "ppm")) %>%
    mutate(sample_type = NA,
           run_or_plate_id = NA,
           measurement_type = "licor",
           subtype = NA,
           subsubtype = NA,
           standard_sample_blank = case_when(
             is.na(std_co2_amount) == FALSE ~ "standard",
             .default = "sample")) %>%
    # Add column for technical replicate number
    group_by(sample_id, analyzed_date) %>%
    mutate(tech_rep_number = seq_along(value)) %>%
    select(sample_id, sample_type, sampled_date, analyzed_date, run_or_plate_id,
           measurement_type, subtype, subsubtype, standard_sample_blank,
           tech_rep_number, std_co2_amount, std_co2_units, value, units)

  return(co2_data_clean)
}
