# Sarah Gao
# October 12, 2021

# This function reads in EA data and finds the
# means and RSDs of all SRMs.

# Calculate means and RSDs for both nitrogen and carbon
# for all SRM samples

library("dplyr")

calculate_srm_stats <- function(cleaned_ea_data) {
  all_srms <- cleaned_ea_data %>%
    filter(sample == "SRM")
  mean_srm_n <- mean(all_srms$n_per)
  rsd_srm_n <- sd(all_srms$n_per) * 100 / mean_srm_n

  mean_srm_c <- mean(all_srms$c_per)
  rsd_srm_c <- sd(all_srms$c_per) * 100 / mean_srm_c

  print(paste("SRM Mean %N:", mean_srm_n))
  print(paste("SRM N RSD:", rsd_srm_n))

  print(paste("SRM Mean %C:", mean_srm_c))
  print(paste("SRM C RSD:", rsd_srm_c))

  srm_stat <- c("mean_n", "rsd_n", "mean_c", "rsd_c")
  values <- c(mean_srm_n, rsd_srm_n, mean_srm_c, rsd_srm_c)
  srm_stats <- data.frame(srm_stat, values)

  str(srm_stats)
  return(srm_stats)
}
