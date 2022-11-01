# This function creates a side by side boxplot of leachate and extract per week
# per N type

# Sarah Gao
# November 1, 2022
# hellosarahgao@gmail.com

library("dplyr")
library("ggplot2")

plot_n_data <- function(n_data_long, y_var) {
  n_type <- str_sub(y_var, end = 3)
  facet_1 <- sym(paste0("leach_", y_var, "_median"))
  facet_2 <- sym(paste0("ext_", y_var, "_median"))
  levels <- c(facet_1, facet_2)
  wk_initial_plot <- n_data_long %>%
    filter(drying_treatment == "initial") %>%
    ggplot(aes(x = factor(cc_treatment, levels = c("no_cc", "w_cc")),
               y = (get(y_var)),
               fill = cc_treatment,
               color = cc_treatment)) +
    geom_boxplot() +
    facet_grid(~ factor(samp_type,
                        levels = levels))  +
    scale_fill_manual(name = NULL, limits = c("no_cc", "w_cc"),
                      values = c("#16B4FF", "#34980D"),
                      labels = c("No Cover Crop", "With Cover Crop")) +
    scale_color_manual(name = NULL, limits = c("no_cc", "w_cc"),
                       values = c("#097CB2", "#195004"),
                       labels = c("No Cover Crop", "With Cover Crop"))

    return(wk_initial_plot)
}
