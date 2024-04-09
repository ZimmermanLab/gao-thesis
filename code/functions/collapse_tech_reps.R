# This function takes in sample-only data with outlier flags and collapses it
# by technical replicates.
# The outlier_flag parameter specifies which outlier you want to filter out
# with the default being no filtering.

# Sarah Gao
# April 8th, 2024
# hellosarahgao@gmail.com

collapse_tech_reps <- function(dataset, outlier_flag = "none"){

  # Filters out tech reps that are extreme outliers
  if(outlier_flag == "extreme"){
    dataset <- dataset %>%
      filter(outlier_flags == "moderate" |
               is.na(outlier_flags))
  }

  # Filters out tech reps that are extreme and moderate outliers
  if(outlier_flag == "moderate"){
    dataset <- dataset %>%
      filter(is.na(outlier_flags))
  }

  # Find mean value for each sample for each test across all tech reps
  collapsed_samples <- dataset %>%
    # Note that this collapses across different dates analyzed
    group_by(sample_id, measurement_type, subtype, subsubtype,
             pre_post_wet, cc_treatment, drying_treatment, sampled_date) %>%
    summarise(mean_value = mean(value, na.rm = TRUE))

  return(collapsed_samples)
}
