# This function takes in the jar assignments csv cleans it up.
# It also merges it with the sampling dates to output a df with all jar ID
# numbers, treatment conditions, and dates sampled.

# The 'need_recoding' parameter accepts either "yes" or "no" and is specifically
# to accommodate for the 2022 experiment in which the same set of CO2 trial jars
# were used for the initial CO2 measurements as the two week CO2 measurements.

# Sarah Gao
# January 19, 2024
# hellosarahgao@gmail.com

library("dplyr")

clean_ids <- function(jar_assignments_file,
                      sampling_dates_file,
                      need_recoding) {

  sampling_dates <- read_csv(sampling_dates_file)

  id_assignments <- read_csv(jar_assignments_file) %>%
    # Add sample_type column to differentiate CO2 only jars
    mutate(sample_type = case_when(
      pre_post_wet == "post_co2" ~ "co2_only",
      .default = "core")) %>%
    # Rename and convert sample IDs to character type
    mutate(sample_id = as.character(sample_no)) %>%
    # Add leading zeros to sample_id for merging
    mutate(sample_id = str_pad(sample_id, width = 3, side = "left", pad = "0")) %>%
    select(-sample_no)

  if (need_recoding == "yes") {
    # Duplicate the two_wk co2_only jars that were used for the initial CO2 sampling
    initial_co2 <- id_assignments %>%
      filter(drying_treatment == "two_wk" &
               sample_type == "co2_only") %>%
      mutate(drying_treatment = "initial")
    # Add back into the id_assignments df
    id_assignments <- id_assignments %>%
      rbind(initial_co2)
  } else {
    next
  }

  # Add sampling dates
  id_assignments <- id_assignments %>%
    left_join(sampling_dates, by =
              c("pre_post_wet", "cc_treatment", "drying_treatment",
                "sample_type"),
            relationship = "many-to-many")

  # Create a "licor" column for merging
  id_assignments <- id_assignments %>%
    mutate(licor = case_when(
      measurement_type == "licor" ~ "yes",
      .default = "no"))

  return(id_assignments)
}
