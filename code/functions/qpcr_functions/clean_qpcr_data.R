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
  files <- dir_ls(path = "data/raw_data/qPCR/",
                         recurse = 1,
                         regex = "fungal -  Quantification Cq Results.csv")
  # Read in data from csv files
  clean_data <- read_csv(files) %>%
    filter(Fluor == "SYBR") %>%
    select(Sample, Cq) %>%
    filter(!(is.na(Sample))) %>%
    filter(!(grepl("Blank", Sample)))

  } else if (micro_type == "bacterial") {
    # Compile list of file paths for bacterial data
    # List of long files from machine 1
    files_long <- dir_ls(path = "data/raw_data/qPCR/",
                    recurse = 1,
                    regex =
                      "bacterial -  Quantification Cq Results.csv$")
    clean_data_long <- read_csv(files_long) %>%
      filter(Fluor == "SYBR") %>%
      select(Sample, Cq) %>%
      filter(!(is.na(Sample))) %>%
      filter(!(grepl("Blank", Sample)))
    # List of short files from machine 2
    files_short <- dir_ls(path = "data/raw_data/qPCR/", recurse = 1,
                          regex = "bacterial.csv$")
    clean_data_short <- read_csv(files_short) %>%
      filter(Fluor == "SYBR") %>%
      select(Sample, Cq) %>%
      filter(!(is.na(Sample))) %>%
      filter(!(grepl("Blank", Sample)))
    # Combine for all bacterial data
    clean_data <- rbind(raw_data_long, raw_data_short)
  }

    # Make sure Cq column is double type
    clean_data$Cq <- as.double(clean_data$Cq)

    # Separate out replicate numbers
    clean_data_all <- clean_data %>%
      filter(cq == "NaN" | is.na(cq)) %>%
      rename("sample_no_full" = Sample, "cq" = Cq) %>%
      mutate("sample_no" = as.numeric(str_sub(sample_no_full, end = 3))) %>%
      mutate("rep_no" = as.numeric(str_sub(sample_no_full, start = -1)))

    # Return cleaned data
    return(clean_data_all)
}
