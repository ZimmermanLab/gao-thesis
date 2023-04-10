# This script cleans data from qPCR assays, taking in raw csv files and
# returning only the sample names and Cq values from SYBR reads.

# Sarah Gao
# October 24, 2022
# hellosarahgao@gmail.com

library("tidyr")
library("dplyr")
library("readr")

clean_qpcr_data <- function(input_dir, micro_type) {
  if (micro_type == "fungal") {
  # Compile list of file paths for fungal data
  files <- dir_ls(path = input_dir, recurse = 1,
                  regex = "fungal -  Quantification Cq Results.csv")
  # Read in data from csv files
  clean_data <- read_csv(files) %>%
    filter(Fluor == "SYBR") %>%
    select(Sample, Cq)

  } else if (micro_type == "bacterial") {
    # Compile list of file paths for bacterial data
    # List of wider files with more columns from machine 1
    files_long <- dir_ls(path = input_dir, recurse = 1,
                         regex =
                           "bacterial -  Quantification Cq Results.csv$")
    clean_data_long <- read_csv(files_long) %>%
      filter(Fluor == "SYBR") %>%
      select(Sample, Cq)
    # List of files with fewer columns from machine 2
    files_short <- dir_ls(path = input_dir, recurse = 1,
                          regex = "bacterial.csv$")
    clean_data_short <- read_csv(files_short) %>%
      filter(Fluor == "SYBR") %>%
      select(Sample, Cq)
    # Combine for all bacterial data
    clean_data <- rbind(clean_data_long, clean_data_short)
  }

    # Make sure Cq column is double type
    clean_data$Cq <- as.double(clean_data$Cq)

    # Separate out replicate numbers and clean up sample number
    clean_data_all <- clean_data %>%
      rename("sample_no" = Sample, "cq" = Cq) %>%
      mutate(rep_no = case_when(
        str_detect(sample_no, "-") ~ str_sub(sample_no, start = -1),
        TRUE ~ NA_character_)) %>%
      mutate(sample_no = case_when(
        str_detect(sample_no, "-") ~ str_sub(sample_no, end = 3),
        TRUE ~ sample_no))

    # Return cleaned data
    return(clean_data_all)
}
