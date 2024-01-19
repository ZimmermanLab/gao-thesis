# Script to plot CO2 in drying soils using Dunn's test
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
source("code/functions/set_plot_themes.R")
set_theme("doc")

#### CO2 IN DRYING SOILS ####

# Read in CO2 data
co2_data <- read_csv("data/cleaned_data/LICOR/samp_medians.csv") %>%
  # Populate day_elapsed column
  mutate(day_elapsed = case_when(date == "2022-02-15" ~ "1",
                                 date == "2022-02-16" ~ "2",
                                 date == "2022-02-17" ~ "3",
                                 date == "2022-02-18" ~ "4",
                                 date == "2022-02-19" ~ "5"))

# Subset to no cc, drying only
nocc_co2 <- co2_data %>%
  filter(drying_treatment == "initial_dry") %>%
  filter(cc_treatment == "no_cc")
wcc_co2 <- co2_data %>%
  filter(drying_treatment == "initial_dry") %>%
  filter(cc_treatment == "w_cc")

# Run Dunn's test and generate plot
nocc_co2_plot <- nocc_co2 %>%
  pairwise_compare(x_value = day_elapsed,
                   y_value = median,
                   p_val_cutoff = 0.1,
                   fill_value = "#16B4FF",
                   col_value = "#097CB2") +
  # Use blue fill and stroke for no_cc palette
  scale_fill_manual(values = "#16B4FF") +
  scale_color_manual(values = "#097CB2") +
  labs(x = "Day of Drying",
       y = "CO2 (ppm)",
       title = "CO2 in Drying Soils")
  # Add test annotation
  # annotation_custom(grob =
  #                    grobTree(textGrob("Dunn's Test",
  #                                      x = .8, y = 0.85,
  #                                      gp = gpar(fontsize = 16,
  #                                                fontfamily = "Helvetica",
  #                                                lineheight = 0.9))))

wcc_co2_plot <- wcc_co2 %>%
  ggplot(aes(x = day_elapsed,
             y = median,
             fill = "#34980D",
             color = "#195004")) +
  geom_boxplot() +
  # Use blue fill and stroke for no_cc palette
  scale_fill_manual(values = "#34980D") +
  scale_color_manual(values = "#195004") +
  scale_y_continuous(labels = label_comma(),limits = c(1, 1000000),
                     trans = pseudo_log_trans(base = 10)) +
  theme(legend.position = "none") +
  labs(x = "Day of Drying",
       y = "CO2 (ppm)",
       title = "CO2 in Drying Soils")


ggsave(nocc_co2_plot, filename = "output/2022/co2/figures/co2_drying_nocc.png",
       width = 8, height = 8, units = "in")
