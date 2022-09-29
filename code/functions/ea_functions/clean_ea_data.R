# This function reads in raw EA data and cleans it up

# Sarah Gao
# October 12, 2021
# hellosarahgao@gmail.com

library("dplyr")
library("readxl")

clean_ea_data <- function(input_file_list) {
  # Read in and clean up EA data
  # Note that reading in the csv will auto name the columns
  # and warn you about it, hence the suppressWarnings()
  ea_results_raw <- input_file_list %>%
    lapply(read_xls, col_names = FALSE) %>%
    bind_rows()

  # Select only sample number, type of sample, and n and c percentages
  ea_results_clean <- ea_results_raw %>%
    select("...2", "...4", "...6", "...12", "...13") %>%
    rename("sample_no" = "...2", "date" = "...4", "type" = "...6",
           "n_mg" = "...12", "c_mg" = "...13") %>%
    drop_na()

  return(ea_results_clean)
}
