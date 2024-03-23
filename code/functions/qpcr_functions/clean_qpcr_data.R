# This script cleans data from qPCR assays, taking in raw csv files and
# returning only the sample names and Cq values from SYBR reads.

# Sarah Gao
# October 24, 2022
# hellosarahgao@gmail.com

library("tidyr")
library("dplyr")
library("readr")

clean_qpcr_data <- function(input_dir) {
  # Compile list of file paths for fungal data
  files_fung <- dir_ls(path = input_dir, recurse = 1,
    regex = "fungal -  Quantification Cq Results.csv")

  # Read in data from csv files
  clean_data_fung <- files_fung %>%
    lapply(read_csv, col_names = TRUE) %>%
    bind_rows(.id = "run_or_plate_id") %>%
    filter(Fluor == "SYBR") %>%
    mutate(measurement_type = "qpcr",
           subtype = "fungal",
           subsubtype = "dna") %>%
    select(Sample, Cq, measurement_type, subtype, subsubtype, run_or_plate_id)

    # Compile list of file paths for bacterial data
    # List of wider files with more columns from machine 1
    files_bact_long <- dir_ls(path = input_dir, recurse = 1,
                         regex =
                           "bacterial -  Quantification Cq Results.csv$")
    clean_data_bact_long <- files_bact_long %>%
      lapply(read_csv, col_names = TRUE) %>%
      bind_rows(.id = "run_or_plate_id") %>%
      filter(Fluor == "SYBR") %>%
      mutate(measurement_type = "qpcr",
             subtype = "bacterial",
             subsubtype = "dna") %>%
      select(Sample, Cq, measurement_type, subtype, subsubtype, run_or_plate_id)


    # List of files with fewer columns from machine 2
    files_bact_short <- dir_ls(path = input_dir, recurse = 1,
                          regex = "bacterial.csv$")
    clean_data_bact_short <- files_bact_short %>%
      lapply(read_csv, col_names = TRUE) %>%
      bind_rows(.id = "run_or_plate_id") %>%
      filter(Fluor == "SYBR") %>%
      mutate(measurement_type = "qpcr",
             subtype = "bacterial",
             subsubtype = "dna") %>%
      select(Sample, Cq, measurement_type, subtype, subsubtype, run_or_plate_id)

    # Combine all types together
    clean_data_all <- clean_data_fung %>%
      rbind(clean_data_bact_long, clean_data_bact_short) %>%
      # Add column for date analyzed
      mutate(analyzed_date = str_extract(run_or_plate_id,
                                         "(?<=qPCR/)\\d{8}")) %>%
      mutate(run_or_plate_id = str_extract(run_or_plate_id, "Run\\d+"))

    # Make sure Cq column is double type
    clean_data_all$Cq <- as.double(clean_data_all$Cq)

    # Separate out replicate numbers and clean up sample number
    clean_data_all <- clean_data_all%>%
      rename(value = Cq,
             sample_id = Sample) %>%
      mutate(tech_rep_number = case_when(
        str_detect(sample_id, "\\d-") ~ str_sub(sample_id, start = -1),
        # NA for blanks
        .default = NA_character_)) %>%
      mutate(sample_id = case_when(
        str_detect(sample_id, "\\d-") ~ str_sub(sample_id, end = 3),
        # Leave as is for blanks
        .default = sample_id)) %>%
      # Add standard / sample / blank column
      mutate(standard_sample_blank = case_when(
        str_detect(sample_id, "^\\d") ~ "sample",
        str_detect(sample_id, "Blank") ~ "blank")) %>%
      # Add columns needed for master dataframe
      mutate(sample_type = "core",
        units = "cq") %>%
      # Rearrange columns
      select(sample_id, sample_type, analyzed_date, run_or_plate_id,
             measurement_type, subtype, subsubtype, standard_sample_blank,
             tech_rep_number, value, units)

    # Make tech rep column integer type
    clean_data_all$tech_rep_number <- as.integer(clean_data_all$tech_rep_number)
    # Replace NaN string with NA values
    is.na(clean_data_all) <- clean_data_all == "NaN"

    # Return cleaned data
    return(clean_data_all)
}
