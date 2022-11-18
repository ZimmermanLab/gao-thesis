# script to plot multiple plots after adding pairwise comparisons
# Initial draft by Naupaka Zimmerman November 17, 2022 nzimmerman@usfca.edu

# load packages
library("readr")
library("dplyr")
library("ggplot2")
library("scales") # for plot label niceness
library("forcats") # for factor manipulation
library("cowplot") # for multiple plots
library("grid") # for multiple plots
library("gridExtra") # for multiple plots

# source functions and themes, see those files for comments etc
source("code/functions/custom_few_theme.R")
source("code/functions/pairwise_compare.R")

# Read in data, subset to bacterial and pre/post-wet, no cc only
bacteria_only <- read_csv("data/samp_medians.csv")

# filter out data for the plots of interest
no_cc_bact <- bacteria_only %>%
  filter(cc_treatment == "no_cc") %>%
  filter(drying_treatment == "one_wk" |
           drying_treatment == "two_wk" |
           drying_treatment == "four_wk") %>%
  # reorder factors to put weeks in order
  mutate(drying_treatment_sorted = fct_relevel(drying_treatment,
                                               "one_wk",
                                               "two_wk",
                                               "four_wk")) %>%
  mutate(drying_treatment_recoded = recode(drying_treatment_sorted,
                                           one_wk = "1 Week",
                                           two_wk = "2 Weeks",
                                           four_wk = "4 Weeks")) %>%
  # multiply by 10000 to make numbers simpler and more readable
  mutate(samp_med_bact_human_read = samp_med_bact * 1e5)

# filter data for first (left-hand) plot, in this case pre-wetting
no_cc_bact %>%
  filter(pre_post_wet == "pre") %>% # filter as needed
  pairwise_compare(x_value = drying_treatment_recoded,
               y_value = samp_med_bact_human_read,
               p_val_cutoff = 0.8) +
  labs(x = NULL, y = NULL) +
  # example for setting limits
  scale_y_continuous(limits = c(0, 8.2)) + # set me; check max in console output
  custom_few_theme() -> pre

# filter data for second (right-hand) plot, in this case post-wetting
no_cc_bact %>%
  filter(pre_post_wet == "post") %>% # filter as needed
  pairwise_compare(x_value = drying_treatment_recoded,
               y_value = samp_med_bact_human_read,
               p_val_cutoff = 0.8) +
  labs(x = NULL, y = NULL) +
  # example for setting limits
  scale_y_continuous(limits = c(0, 8.2)) + # set me; check max in console output
  custom_few_theme() -> post

# put the two subplots together. Note you could add more if you like
combined_plot <- plot_grid(pre, post,
          labels = c('Pre', 'Post'),
          label_size = 12, vjust = 0) +

  # have to adjust this on top to make space for subplot labels
  theme(plot.margin = unit(c(0.5, 0, 0, 0), "cm")) # top, right, bottom, left

# combined label for x axis
x.grob <- textGrob("Drought Treatment Duration",
                   gp = gpar(fontface = "bold",
                             fontsize = 15))

# combined label for y axis
y.grob <- textGrob("Proportional DNA Amount",
                   gp = gpar(fontface = "bold",
                             fontsize = 15),
                   rot = 90)

# add all the things to plot
grid.arrange(arrangeGrob(combined_plot, left = y.grob, bottom = x.grob))

