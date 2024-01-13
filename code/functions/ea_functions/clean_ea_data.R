# This function reads in raw EA data and cleans it up

# Sarah Gao
# October 12, 2021
# hellosarahgao@gmail.com

library("dplyr")
library("readxl")
library("tidyr")
library("stringi")
library("stringr")

clean_ea_data <- function(input_file_list) {
  # Read in and clean up EA data
  # Note that reading in the csv will auto name the columns
  # and warn you about it, hence the suppressWarnings()
  ea_results_raw <- input_file_list %>%
    lapply(read_xls, col_names = FALSE) %>%
    bind_rows(.id = "run_or_plate_id") %>%
    mutate(run_or_plate_id = str_extract(run_or_plate_id, "Run\\d"))

  # FOR PERCENTAGE DATA:
  # Select only sample number, type of sample, and n and c percentages
  if (stri_detect_fixed(input_file_list[1], "percent") == TRUE) {
    ea_results_clean <- ea_results_raw %>%
      select("...2", "...4", "...6", "...12", "...13", run_or_plate_id) %>%
      rename("sample_id" = "...2", "analyzed_date" = "...4", "type" = "...6",
             "nitrogen_pcnt" = "...12", "carbon_pcnt" = "...13") %>%
      # Filter out any samples and/or SRM runs that were 0
      filter(!(str_detect(sample_id, "SG") & nitrogen_pcnt == 0)) %>%
      filter(!(str_detect(sample_id, "SRM") & nitrogen_pcnt == 0)) %>%
      # Pivot to long format
      pivot_longer(cols = c(nitrogen_pcnt, carbon_pcnt),
                   names_to = "subtype", values_to = "value")
  }
  # FOR RATIO DATA:
  else if (stri_detect_fixed(input_file_list[1], "ratio") == TRUE) {
      ea_results_clean <- ea_results_raw %>%
        select("...2", "...4", "...6", "...13", run_or_plate_id) %>%
        rename("sample_id" = "...2", "analyzed_date" = "...4", "type" = "...6",
               "c_n_ratio" = "...13") %>%
      # Filter out any samples and/or SRM runs that were 0
      filter(!(str_detect(sample_id, "SG") & c_n_ratio == 0)) %>%
      filter(!(str_detect(sample_id, "SRM") & c_n_ratio == 0)) %>%
      # Pivot to long format
      pivot_longer(cols = c_n_ratio,
                   names_to = "subtype", values_to = "value")
  }

  # Clean up sample names
  ea_results_clean <- ea_results_clean %>%
    mutate(sample_id = case_when(
    str_detect(sample_id, "SG") ~
      str_sub(sample_id, start = -3),
    TRUE ~ sample_id)) %>%
    # Reformat date column
    mutate(analyzed_date = strftime(as.Date(
      analyzed_date, "%m/%d/%Y"),"%Y%m%d")) %>%
    # Filter out NA values
    drop_na() %>%
    # Add sample type column
    mutate(sample_type = "core") %>%
    # Add measurement type column
    mutate(measurement_type = "ea") %>%
    # Add standard / sample / blank column
    mutate(standard_sample_blank = case_when(
      type == "STD" ~ "standard",
      (type == "UNK" & str_detect(sample_id, "B")) ~ "blank",
      (type == "UNK" & str_detect(sample_id, "SRM")) ~ "standard",
      .default = "sample")) %>%
    # Add empty columns needed for master dataframe
    mutate(units = NA,
           subsubtype = NA) %>%
    # Add column for technical replicate number
    group_by(sample_id, analyzed_date, subtype) %>%
    mutate(tech_rep_number = seq_along(value)) %>%
    # Rearrange columns and drop "type" column
    select(sample_id, sample_type, analyzed_date, run_or_plate_id,
           measurement_type, subtype, subsubtype, standard_sample_blank,
           tech_rep_number, value, units)


  return(ea_results_clean)
}
