# Sarah Gao
# October 12, 2021

# This function reads in raw EA data and cleans it up
# to be used in downstream analyses

library("dplyr")

clean_ea_data <- function(input_file_name) {
  # Read in and clean up EA data
  # Note that reading in the csv will auto name the columns
  # and warn you about it, hence the suppressWarnings()
  ea_results_raw <- suppressWarnings(readr::read_csv(input_file_name, col_names = FALSE))
  ea_results_clean <- ea_results_raw %>%
    select("X2", "X4", "X6", "X7", "X12", "X13")

  ea_results_clean <- ea_results_clean[rowSums(
    is.na(ea_results_clean)) != ncol(ea_results_clean), ] %>%
    rename("sample" = "X2", "date" = "X4", "type" = "X6", "weight" = "X7",
           "n_per" = "X12", "c_per" = "X13") %>%
    mutate(pos = row_number())

  # Reorder the columns so position is the first column
  ea_results_clean <- ea_results_clean[colnames(ea_results_clean)[c(7, 1:6)]]

  readr::write_csv(ea_results_clean, paste0("data/cleaned_data/",
                                            tools::file_path_sans_ext(basename(
                                              input_file_name)),
                                            "_cleaned.csv"
                                            ))
  return(ea_results_clean)
}
