# This script takes cleaned data from qPCR assays and finds the means and SDs
# per sample.

# Sarah Gao
# October 25, 2022
# hellosarahgao@gmail.com

library("tidyr")
library("dplyr")
library("readr")

qpcr_sample_stats <- function(clean_data) {
  # Calculate means and SDs per sample
    qpcr_stats <- clean_data %>%
      group_by(sample_no) %>%
      summarize(mean_cq = mean(cq),
                sd_cq = sd(cq))
    return(qpcr_stats)
}
