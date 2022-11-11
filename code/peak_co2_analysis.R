# This script analyzes and plots peak CO2 levels taken from LICOR data

# Sarah Gao
# November 2, 2022
# hellosarahgao@gmail.com

library(ggplot2)
library(scales)

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
nocc_peaks_all <- day_med_all %>%
  filter(cc_treatment == "no_cc") %>%
  group_by(drying_treatment) %>%
  top_n(1, day_med) %>%
  unique() %>%
  arrange(factor(drying_treatment, levels = c("one_wk", "two_wk", "four_wk")))
# Subset w/o cc for plotting + stats
nocc_peaks_subset <- samp_all %>%
  filter(date %in% nocc_peaks_all$date) %>%
  filter(cc_treatment == "no_cc")

# With cover crop
# Find max peak per week
wcc_peaks_all <- day_med_all %>%
  filter(cc_treatment == "w_cc") %>%
  group_by(drying_treatment) %>%
  top_n(1, day_med) %>%
  unique() %>%
  arrange(factor(drying_treatment, levels = c("one_wk", "two_wk", "four_wk")))
# Subset w/ cc for plotting + stats
wcc_peaks_subset <- samp_all %>%
  filter(date %in% wcc_peaks_all$date) %>%
  filter(cc_treatment == "w_cc")

# Remerge them back into one
peaks_all_data <- nocc_peaks_subset %>%
  rbind(wcc_peaks_subset)

# Set facet labels
facet_cc_names <- as_labeller(c("no_cc" = "Without Cover Crop",
                                "w_cc" = "With Cover Crop"))
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
