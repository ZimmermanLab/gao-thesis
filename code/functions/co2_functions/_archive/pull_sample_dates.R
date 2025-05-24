# This function takes names of LICOR CO2 files and outputs a dataframe showing
# CO2 sampling dates for each sample_id.

# Sarah Gao
# January 16, 2024
# hellosarahgao@gmail.com

get_co2_sample_dates <- function(files_directory) {

  # Create list of file paths of CO2 data files
  files_list <- list.files(path = files_directory, recursive = FALSE,
                           pattern = "*.txt")
  files_list <- subset(files_list, str_detect(files_list, "extract") == FALSE)

  # Extract only the dates and sample numbers
  dates_sample_only <- data.frame("sampled_date" = NA, "sample_id" = NA,
                                   "file_name" = files_list)
  dates_sample_only <- dates_sample_only %>%
    mutate(sampled_date =
             str_extract(file_name, pattern = "^\\d{8}"),
           sample_id =
             str_extract(file_name, pattern = "(?<=^\\d{8}_)[:alnum:]+")) %>%
    # Remove technical replicates
    distinct(sampled_date, sample_id)

# Add CO2 timepoint hour column
  dates_sample_only <- dates_sample_only %>%
    mutate(co2_timepoint_h = )

  return(dates_sample_only)
}
