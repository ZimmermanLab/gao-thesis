# Sarah Gao
# October 12, 2021

# This function reads in raw EA data and cleans it up
# to be used in downstream analyses

library("dplyr")

input_file_name <- "data/raw_data/EA_CN/20210727/210727_Run.csv"

clean_ea_data <- function(input_file_name) {
  # Read in and clean up EA data
  # Note that reading in the csv will auto name the columns
  # and warn you about it, hence the suppressWarnings()
  ea_results_raw <- suppressWarnings(
    readr::read_csv(input_file_name, col_names = FALSE))

  # Select only sample number, type of sample, and n and c percentages
  ea_results_clean <- ea_results_raw %>%
    select("X2", "X6", "X12", "X13") %>%
    rename("sample_no" = "X2", "type" = "X6",
           "n_per" = "X12", "c_per" = "X13") %>%
    drop_na() %>%
    mutate(pos = row_number())

  # Reorder the columns so position is the first column
  ea_results_clean <- ea_results_clean[colnames(ea_results_clean)[c(5, 1:4)]]

  readr::write_csv(ea_results_clean, paste0("data/cleaned_data/",
                                            tools::file_path_sans_ext(basename(
                                              input_file_name)),
                                            "_cleaned.csv"
                                            ))
  return(ea_results_clean)
}
