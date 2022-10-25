# This script cleans data from qPCR assays, taking in raw csv files and
# returning only the sample names and Cq values from SYBR reads.

# Sarah Gao
# October 24, 2022
# hellosarahgao@gmail.com

library("tidyr")
library("dplyr")
library("readr")

clean_qpcr_data <- function(input_files) {
    # Read in data from csv files
    raw_data <- read_csv(input_files) %>%
      rbind
    # Clean up compiled data and remove Blanks
    clean_data <- raw_data %>%
      filter(Fluor == "SYBR") %>%
      select(Sample, Cq) %>%
      filter(!(is.na(Sample))) %>%
      filter(!(grepl("Blank", Sample)))
    # Make sure Cq column is double type
    clean_data$Cq <- as.double(clean_data$Cq)

    # Separate out replicate numbers
    clean_data_all <- clean_data %>%
      rename("sample_no_full" = Sample, "cq" = Cq) %>%
      mutate("sample_no" = as.numeric(str_sub(sample_no_full, end = 3))) %>%
      mutate("rep_no" = as.numeric(str_sub(sample_no_full, start = -1)))

    # Return cleaned data
    return(clean_data_all)
}
