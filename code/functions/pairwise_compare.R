# This function takes in a dataset and runs Dunn's test on pairwise comparisons
# The output is a plot with stats displayed.

# Originally written by Naupaka Zimmerman on November 17th, 2022
# Sarah Gao
# hellosarahgao@gmail.com

library("apexcharter")
library("pairwiseComparisons")
library("ggplot2")
library("dplyr")
library("ggpubr")
library("rlang")

# can take bare (unquoted) column names as arguments
pairwise_compare <- function(dataset, x_value, y_value,
                             fill_value, col_value,
                             p_val_cutoff = 0.1,
                             y_pos = NA) {

  # quote the bare column names
  x_value <- enquo(x_value)
  y_value <- enquo(y_value)

  # calculate pairwise p-values for all pairs using pairwiseComparisons package
  # !! syntax is to unquote bare column names
  comparison_results <- pairwise_comparisons(data = dataset,
                                             x = !! x_value,
                                             y = !! y_value,
                                             type = "nonparametric",
                                             paired = FALSE,
                                             p.adjust.method = "fdr")

  # add column with p-value thresholds
  comparison_results_codes <- comparison_results %>%
    mutate(p.label = case_when(
      p.value <= 0.001 ~ "***",
      p.value > 0.001 & p.value <= 0.01 ~ "**",
      p.value > 0.01 & p.value <= 0.05 ~ "*",
      p.value > 0.05 & p.value <= 0.1 ~ ".",
      p.value > 0.1 ~ "NS"))

  # filter table to only contain rows with p values below chosen threshold
  filtered_results_df <- comparison_results_codes %>%
    filter(p.value < p_val_cutoff)

  # output results to console for checking or inclusion in manuscript
  print("Pairwise comparisons below alpha threshhold after correction for MHT:")
  print(filtered_results_df[c(1, 2, 4, 12)])

  # automatically determine
  y_max <- max(pull(dataset, !! y_value)) # get max y value in dataset
  y_min <- min(pull(dataset, !! y_value)) # get min y value in dataset
  range <- y_max - y_min # calculate range
  interval <- range/6 # use range to calculate spacing interval

  # create comparison bar y heights using above values
  if(is.na(y_pos)) {
    y_positions <- seq(from = y_max + interval,
                       by = interval,
                       length.out = ifelse(nrow(filtered_results_df),
                                           nrow(filtered_results_df),
                                           1))
  } else if(!is.na(y_pos)) {
    y_positions <- y_pos
  }

  # let the user know the max y for this plot by output to console
  print("Maximum y axis position in this plot:")
  print(max(y_positions))

  # make a ggplot with these values
  constructed_plot <- dataset %>%
    ggplot(aes(x = !! x_value, y = !! y_value, fill = !! fill_value,
               color = !! col_value)) +
    geom_boxplot() +
    ggpubr::stat_pvalue_manual(data = filtered_results_df,
                               label = "p.label",
                               y.position = y_positions, size = 8) +
    theme(legend.position = "none")

  # return the ggplot object
  return(constructed_plot)
}
