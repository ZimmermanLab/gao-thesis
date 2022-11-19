# This script takes in CO2 data and creates plots of rewetting timecourses
# that include Dunn's test stats

# Sarah Gao
# November 18th, 2022
# hellosarahgao@gmail.com

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
  labs(x = NULL, y = NULL) +
  scale_fill_manual(values = "#16B4FF") +
  scale_color_manual(values = "#097CB2") +
  scale_y_continuous(limits = c(0, 5840))

# Filter for two weeks for second plot
nocc_two <- co2_nocc_rewet %>%
  filter(drying_treatment == "two_wk") %>%
  pairwise_compare(dataset = ., x_value = day_elapsed,
                   y_value = median,
                   p_val_cutoff = 0.1,
                   fill_value = "#16B4FF",
                   col_value = "#097CB2") +
  labs(x = NULL, y = NULL) +
  scale_fill_manual(values = "#16B4FF") +
  scale_color_manual(values = "#097CB2") +
  scale_y_continuous(limits = c(0, 9200))

# Filter for four weeks for third plot
nocc_four <- co2_nocc_rewet %>%
  filter(drying_treatment == "four_wk") %>%
  pairwise_compare(dataset = ., x_value = day_elapsed,
                   y_value = median,
                   p_val_cutoff = 0.1,
                   fill_value = "#16B4FF",
                   col_value = "#097CB2") +
  labs(x = NULL, y = NULL) +
  scale_fill_manual(values = "#16B4FF") +
  scale_color_manual(values = "#097CB2") +
  scale_y_continuous(limits = c(0, 5530))

# put the subplots together
combined_plot <- plot_grid(nocc_one, nocc_two, nocc_four,
                           labels = c("One Week", "Two Weeks", "Four Weeks"),
                           label_size = 12, nrow = 1, align = "hv", label_x = 0.38) +
  # have to adjust this on top to make space for subplot labels
  theme(plot.margin = unit(c(0.5, 0, 0, 0), "cm")) # top, right, bottom, left

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

# add all the things to plot
grid.arrange(arrangeGrob(combined_plot, left = y.grob, bottom = x.grob))

