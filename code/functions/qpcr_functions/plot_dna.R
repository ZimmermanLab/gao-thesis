# This function creates boxplots from DNA data by taking in
# proportional concentration data and using the median

# Sarah Gao
# October 25, 2022
# hellosarahgao@gmail.com

library("readr")
library("tidyr")
library("dplyr")
library("ggplot2")

plot_dna <- function(treatment_stats, type) {
  boxplot <- treatment_stats %>%
    group_by(pre_post_wet, cc_treatment, drying_treatment) %>%
    ggplot(aes(x = pre_post_wet,
               y = prop_conc_norm)) +
      geom_boxplot() +
      facet_grid(. ~ factor(drying_treatment,
                            levels = c("initial", "one_wk", "two_wk",
                                       "four_wk", "all_dry"))) +
      scale_x_discrete(limits = c("all_dry", "initial", "cw", "pre", "post"),
                       labels = c("All-Dry", "Initial", "Constant", "Pre-Wet",
                                  "Post-Wet"))
return(boxplot)
}
