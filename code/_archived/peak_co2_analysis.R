# This script analyzes and plots peak CO2 levels taken from LICOR data

# Sarah Gao
# November 2, 2022
# hellosarahgao@gmail.com

library(ggplot2)
library(scales)
library(grid)

source("code/functions/pairwise_compare.R")
source("code/functions/set_plot_themes.R")
set_theme("doc")

samp_median <- read_csv("data/cleaned_data/LICOR/samp_medians.csv")

# ALL SAMPLES, INCLUDING CO2-ONLY (minus initial dry)
samp_all <- samp_median %>%
  filter(drying_treatment != "initial_dry")

# Pool samples per day
day_med_all <- samp_all %>%
  group_by(cc_treatment, drying_treatment, date) %>%
  summarise(day_med = median(median), day_iqr = IQR(median))

# Without cover crop
# Find max peak per week
peaks_all <- day_med_all %>%
  group_by(drying_treatment, cc_treatment) %>%
  top_n(1, day_med) %>%
  unique() %>%
  arrange(factor(drying_treatment, levels = c("one_wk", "two_wk", "four_wk")))

# Subset w/o cc for plotting + stats
nocc_peaks_sum <- peaks_all %>%
  filter(cc_treatment == "no_cc")
nocc_peaks_subset <- samp_all %>%
  filter(cc_treatment == "no_cc",
         date %in% nocc_peaks_sum$date)

# With cover crop
# Subset w/ cc for plotting + stats
wcc_peaks_sum <- peaks_all %>%
  filter(cc_treatment == "w_cc")
wcc_peaks_subset <- samp_all %>%
  filter(date %in% wcc_peaks_sum$date) %>%
  filter(cc_treatment == "w_cc")

# Merge back together for side by side plot
all_peaks <- wcc_peaks_subset %>%
  rbind(nocc_peaks_subset)

# Save out peak data
write_csv(all_peaks, "data/cleaned_data/LICOR/all_peaks.csv")

# Set facet names
facet_cc_names <- as_labeller(c("no_cc" = "No Cover Crop",
                                "w_cc" = "With Cover Crop"))

# Plot cc_treatment side by side with KW stats
peak_all_plot <- all_peaks %>%
  mutate(drying_treatment = factor(drying_treatment,
                                   levels = c(
                                     "one_wk", "two_wk", "four_wk"))) %>%
  ggplot(aes(x = drying_treatment,
             y = median)) +
  geom_boxplot(aes(fill = cc_treatment,
                   color = cc_treatment)) +
  labs(x = "Drying Time",
       y = "Peak CO2 (ppm)",
       title = "Peak CO2 After Rewetting") +
  facet_wrap(~ cc_treatment,
             labeller = facet_cc_names) +
  theme(legend.position = "none") +
  scale_color_manual(limits = c("no_cc", "w_cc"),
                     values = c("#097CB2", "#195004")) +
  scale_fill_manual(limits = c("no_cc", "w_cc"),
                    values = c("#16B4FF", "#34980D")) +
  scale_y_continuous(labels = label_comma(),
                     trans = "log10") +
  scale_x_discrete(labels = c("One Week", "Two Weeks", "Four Weeks")) +
  theme(panel.spacing = unit(2, "lines"))
  # Add overall Kruskal-Wallis p-values
  # stat_compare_means(label.x = 2.25, label.y = 4,
  #                   family = "Helvetica",
  #                   size = 6)
ggsave(peak_all_plot, filename =
         "output/2022/co2/figures/peak_rewet.png",
       width = 14, height = 8, units = "in")

# Create pairwise plot + Dunn's test stats on no cc
nocc_plot <- nocc_peaks_subset %>%
  # Refactor drying_treatment for proper ordering
  mutate(drying_treatment = factor(drying_treatment,
                                   levels = c(
                                     "one_wk", "two_wk", "four_wk"))) %>%
  pairwise_compare(x_value = drying_treatment,
                   y_value = median,
                   fill_value = "#16B4FF",
                   col_value = "#097CB2",
                   p_val_cutoff = 0.1) +
  scale_fill_manual(values = "#16B4FF") +
  scale_color_manual(values = "#097CB2") +
  scale_y_continuous(labels = label_comma(),
                     limits = c(1800, 6300)) +
  scale_x_discrete(labels = c("One Week", "Two Weeks", "Four Weeks")) +
  labs(x = NULL,
       y = "Peak CO2 (ppm)",
       title = "No Cover Crop") +
  theme(plot.title = element_text(size = 20),
        plot.margin = margin(c(0, 0, 10, 30)))

# Create pairwise plot + Dunn's test stats on w cc
wcc_plot <- wcc_peaks_subset %>%
  # Refactor drying_treatment for proper ordering
  mutate(drying_treatment = factor(drying_treatment,
                                   levels = c(
                                     "one_wk", "two_wk", "four_wk"))) %>%
  pairwise_compare(x_value = drying_treatment,
                   y_value = median,
                   fill_value = "#16B4FF",
                   col_value = "#097CB2",
                   p_val_cutoff = 0.1) +
  scale_fill_manual(values = "#34980D") +
  scale_color_manual(values = "#195004") +
  scale_y_continuous(labels = label_comma(),
                     limits = c(40000, 330000)) +
  scale_x_discrete(labels = c("One Week", "Two Weeks", "Four Weeks")) +
  labs(x = NULL,
       y = NULL,
       title = "With Cover Crop") +
  theme(plot.title = element_text(size = 20),
        plot.margin = margin(c(0, 30, 0, 10)))

# Combine plots together
patchwork_plot <- (nocc_plot + wcc_plot) +
  plot_annotation(
    title = "Peak CO2 After Rewetting",
    theme = theme(plot.title = element_text(size = 24),
                  plot.margin = margin(c(0, 0, 0, 0))))

# combined label for x axis
x.grob <- textGrob("Drying Time",
                   gp = gpar(fontface = "bold",
                             fontsize = 20,
                             fontfamily = "Georgia"))

# Add axes labels to plot
pw_plot <- patchworkGrob(patchwork_plot)
peak_rewet_plot <- grid.arrange(arrangeGrob(pw_plot,
                                            bottom = x.grob),
                                vp = viewport(width = 1, height = 0.95))
ggsave(peak_rewet_plot, filename = "output/2022/co2/figures/peak_rewet.png",
       width = 14, height = 8, units = "in")



# Plot max peak per week
peaks_plot_all <- peaks_all_data %>%
  ggplot(aes(x = drying_treatment,
         y = median,
         fill = cc_treatment,
         color = cc_treatment)) +
  geom_boxplot() +
  facet_wrap(~ cc_treatment, scales = "free", labeller = facet_cc_names) +
  scale_color_manual(limits = c("no_cc", "w_cc"),
                     values = c("#097CB2", "#195004")) +
  scale_fill_manual(limits = c("no_cc", "w_cc"),
                    values = c("#16B4FF", "#34980D")) +
  scale_y_continuous(labels = label_comma()) +
  scale_x_discrete(limits = c("one_wk", "two_wk", "four_wk"),
                   labels = c("One Week", "Two Weeks", "Four Weeks")) +
  theme(panel.spacing = unit(2, "lines"),
        legend.position = "none") +
  labs(x = "Drying Time",
       y = "Peak Co2 (ppm)",
       title = "Peak CO2 After Rewetting")
ggsave(peaks_plot_all, filename = "output/2022/co2/figures/peak_all.png",
       width = 12, height = 8, units = "in")

# Run stats on effect of drying time on peak CO2 per cc treatment
# No cc
nocc_peaks_stats <- nocc_peaks_subset %>%
  kruskal.test(data = ., median ~ drying_treatment)
# W cc
wcc_peaks_stats <- wcc_peaks_subset %>%
  kruskal.test(data = ., median ~ drying_treatment)

# Run stats on effect of cc treatment
# In dried soils
dry_cc_stats <- samp_median %>%
  filter(drying_treatment == "initial_dry") %>%
  kruskal.test(data = ., median ~ cc_treatment)
# In post-wet soils
rewet_cc_stats <- peaks_all_data %>%
  kruskal.test(data = ., median ~ cc_treatment)

# CO2 ONLY JARS
# Pool samples per day
day_med_co2 <- samp_all %>%
  group_by(cc_treatment, drying_treatment, date) %>%
  filter(pre_post_wet == "post_co2") %>%
  summarise(day_med = median(median), day_iqr = IQR(median))

# Without cover crop
# Find max peak per week
nocc_peaks_co2 <- day_med_co2 %>%
  filter(cc_treatment == "no_cc") %>%
  group_by(drying_treatment) %>%
  top_n(1, day_med) %>%
  unique() %>%
  arrange(factor(drying_treatment, levels = c("one_wk", "two_wk", "four_wk")))
# Subset for stats
nocc_peaks_co2_subset <- samp_all %>%
  filter(pre_post_wet == "post_co2") %>%
  filter(date %in% wcc_peaks_co2$date) %>%
  filter(cc_treatment == "no_cc")

# With cover crop
# Find max peak per week
wcc_peaks_co2 <- day_med_co2 %>%
  filter(cc_treatment == "w_cc") %>%
  group_by(drying_treatment) %>%
  top_n(1, day_med) %>%
  unique() %>%
  arrange(factor(drying_treatment, levels = c("one_wk", "two_wk", "four_wk")))
# Subset for stats
wcc_peaks_co2_subset <- samp_all %>%
  filter(pre_post_wet == "post_co2") %>%
  filter(date %in% wcc_peaks_co2$date) %>%
  filter(cc_treatment == "w_cc")

# Run stats on drying time effect on peak CO2 upon rewetting
# No cc
nocc_peaks_co2_stats <- nocc_peaks_co2_subset %>%
  kruskal.test(data = ., median ~ drying_treatment)
# W cc
wcc_peaks_stats <- wcc_peaks_co2_subset %>%
  kruskal.test(data = ., median ~ drying_treatment)

# Remerge them back into one
peaks_co2_data <- nocc_peaks_co2_subset %>%
  rbind(wcc_peaks_co2_subset)
