# This script takes in CO2 data and creates plots of rewetting timecourses
# that include Dunn's test stats

# Sarah Gao
# November 18th, 2022
# hellosarahgao@gmail.com

library("ggplot2")
library("dplyr")
library("patchwork")
library("gridExtra")

# Source function and set plot themes
source("code/functions/set_plot_themes.R")
set_theme("pres")
source("code/functions/custom_few_theme.R")

# Source function for running Dunn's test
source("code/functions/pairwise_compare.R")

# Bring in CO2 data and subset to no cc, rewetting data only
co2_data <- read_csv("data/cleaned_data/LICOR/samp_medians.csv")
co2_nocc_rewet <- co2_data %>%
  filter(drying_treatment != "initial_dry") %>%
  filter(pre_post_wet == "post_co2") %>%
  filter(cc_treatment == "no_cc") %>%
  mutate(day_elapsed = as.character(day_elapsed))

# Filter for one week for first plot
nocc_one <- co2_nocc_rewet %>%
  filter(drying_treatment == "one_wk") %>%
  pairwise_compare(dataset = ., x_value = day_elapsed,
                   y_value = median,
                   p_val_cutoff = 0.1,
                   fill_value = "#16B4FF",
                   col_value = "#097CB2") +
  labs(x = NULL, y = NULL,
       title = "One Week") +
  scale_fill_manual(values = "#16B4FF") +
  scale_color_manual(values = "#097CB2") +
  scale_y_continuous(limits = c(0, 9200),
                     labels = label_comma()) +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
        plot.title = element_text(size = 20))

# Filter for two weeks for second plot
nocc_two <- co2_nocc_rewet %>%
  filter(drying_treatment == "two_wk") %>%
  pairwise_compare(dataset = ., x_value = day_elapsed,
                   y_value = median,
                   p_val_cutoff = 0.1,
                   fill_value = "#16B4FF",
                   col_value = "#097CB2") +
  labs(x = NULL, y = NULL,
       title = "Two Weeks") +
  scale_fill_manual(values = "#16B4FF") +
  scale_color_manual(values = "#097CB2") +
  scale_y_continuous(limits = c(0, 9200)) +
  theme(plot.margin = unit(c(0, 0, 0, 0.3), "cm"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(size = 16))

# Filter for four weeks for third plot
nocc_four <- co2_nocc_rewet %>%
  filter(drying_treatment == "four_wk") %>%
  pairwise_compare(dataset = ., x_value = day_elapsed,
                   y_value = median,
                   p_val_cutoff = 0.1,
                   fill_value = "#16B4FF",
                   col_value = "#097CB2") +
  labs(x = NULL, y = NULL,
       title = "Four Weeks") +
  scale_fill_manual(values = "#16B4FF") +
  scale_color_manual(values = "#097CB2") +
  scale_y_continuous(limits = c(0, 9200)) +
  theme(plot.margin = unit(c(0, 0, 0, 0.3), "cm"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(size = 16)) +
  # Add test annotation
  annotate("text", x = 4.5, y = 9000, label = "Dunn's Test",
           size = 5, family = "Helvetica")

# Combine plots together
patchwork_plot <- (nocc_one + nocc_two + nocc_four) +
  plot_annotation(
    title = "CO2 After Rewetting Without Cover Crop",
    theme = theme(plot.title = element_text(size = 24),
                  plot.margin = margin(unit(c(0, 0, 15, 15), "cm"))))

# combined label for x axis
x.grob <- textGrob("Days After Rewetting",
                   gp = gpar(fontface = "bold",
                             fontsize = 20,
                             fontfamily = "Georgia"))

# combined label for y axis
y.grob <- textGrob("CO2 (ppm)",
                   gp = gpar(fontface = "bold",
                             fontsize = 20,
                             fontfamily = "Georgia"),
                   rot = 90)

# Add axes labels to plot
pw_plot <- patchworkGrob(patchwork_plot)
nocc_rewet_plot <- grid.arrange(arrangeGrob(pw_plot,
                                left = y.grob,
                                bottom = x.grob),
                                vp = viewport(width = 0.95, height = 0.95))
ggsave(nocc_rewet_plot, filename = "output/2022/co2/figures/co2_rewet_nocc.png",
       width = 14, height = 8, units = "in")
