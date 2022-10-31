# This function reads in raw EA data and cleans it up

# Sarah Gao
# October 12, 2021
# hellosarahgao@gmail.com

library("dplyr")
library("readxl")
library("tidyr")
library("stringi")

clean_ea_data <- function(input_file_list) {
  # Read in and clean up EA data
  # Note that reading in the csv will auto name the columns
  # and warn you about it, hence the suppressWarnings()
  ea_results_raw <- input_file_list %>%
    lapply(read_xls, col_names = FALSE) %>%
    bind_rows()

  # FOR PERCENTAGE DATA:
  # Select only sample number, type of sample, and n and c percentages
  if (stri_detect_fixed(input_file_list[1], "percent") == TRUE) {
    ea_results_clean <- ea_results_raw %>%
      select("...2", "...4", "...6", "...12", "...13") %>%
      rename("sample_no" = "...2", "date" = "...4", "type" = "...6",
             "n_per" = "...12", "c_per" = "...13") %>%
      drop_na() %>%
      # Filter out any samples and/or SRM runs that were 0
      filter(!(str_detect(sample_no, "SG") & n_per == 0)) %>%
      filter(!(str_detect(sample_no, "SRM") & n_per == 0))

  } else if (stri_detect_fixed(input_file_list[1], "ratio") == TRUE) {
      ea_results_clean <- ea_results_raw %>%
        select("...2", "...4", "...6", "...13") %>%
        rename("sample_no" = "...2", "date" = "...4", "type" = "...6",
               "c_n_ratio" = "...13") %>%
        drop_na() %>%
      # Filter out any samples and/or SRM runs that were 0
      filter(!(str_detect(sample_no, "SG") & c_n_ratio == 0)) %>%
      filter(!(str_detect(sample_no, "SRM") & c_n_ratio == 0))
  }
  ea_results_clean <- ea_results_clean %>%
    filter(!str_detect(sample_no, "BLANK")) %>%
    filter(!str_detect(sample_no, "Blank")) %>%
    filter(!str_detect(sample_no, "Bypass")) %>%
    filter(!str_detect(sample_no, "^ASP"))
  # Clean up sample names
  ea_results_clean$sample_no <- case_when(
    str_detect(ea_results_clean$sample_no, "SG") ~
      str_sub(ea_results_clean$sample_no, start = -3),
    str_detect(ea_results_clean$sample_no, "SRM") ~ ea_results_clean$sample_no)

  return(ea_results_clean)
}
