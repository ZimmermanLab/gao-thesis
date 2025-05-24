# This function takes in cleaned data with non-sample data (e.g. blanks and
# standards) and removes the non-sample data.

# Sarah Gao
# April 8th, 2024
# hellosarahgao@gmail.com

isolate_samples <- function(clean_data) {
  samples_only <- clean_data %>%
    filter(standard_sample_blank == "sample")

  return(samples_only)
}
