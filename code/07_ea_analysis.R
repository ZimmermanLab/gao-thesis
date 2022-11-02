# This script takes in EA data, evaluates standards (SRMs) precision over
# time, and analyzes + plots CN soil data.

# Sarah Gao
# July 28, 2021
# hellosarahgao@gmail.com

library("readr")
library("tidyr")
library("dplyr")
library("ggplot2")
library("scales")
library("readxl")
library("fs")

############

# Compile list of file paths of percentage EA data
files_percent <- dir_ls(path = "data/raw_data/EA_CN/2022/",
                recurse = 1,
                regex = "\\w+_Run\\d_(repeat_)*(\\d{2}_)*percent.(xls|XLS)")

# Read in and clean up percentage EA data
# Filter out any weird sample runs that had 0
source("code/functions/ea_functions/clean_ea_data.R")
cn_percent_clean <- clean_ea_data(files_percent)

# Get outlier flags (>1.5x IQR = moderate, >3x IQR = extreme)
source("code/functions/ea_functions/flag_outliers.R")
percent_flags <- flag_outliers(cn_percent_clean, "percent")

# Save out outliers only
percent_outliers <- percent_flags %>%
  filter(!(is.na(outlier_flags_per)))

# SRMs
# Examine the SRM data to see if there was any drift / trend
# over time and to determine if we need any correcting factors.

# Calculate means and RSDs for both nitrogen and carbon
# for all SRM samples
# From Calla: RSDs for both N and C should be 5-10%
source("code/functions/ea_functions/ea_calculate_srm_stats.R")
srm_stats_all <- calculate_srm_stats(percent_flags)
srm_stats_no_ext <- percent_flags %>%
  filter(outlier_flags_per == "moderate" |
           is.na(outlier_flags_per)) %>%
  calculate_srm_stats()
srm_stats_no_mod <- percent_flags %>%
  filter(is.na(outlier_flags_per)) %>%
  calculate_srm_stats()

############

# SAMPLES

# Compile C:N ratio data
files_ratio <- dir_ls(path = "data/raw_data/EA_CN/2022/",
                      recurse = 1,
                      regex = "\\w+_Run\\d_(repeat_)*(\\d{2}_)*ratio.(xls|XLS)")
cn_ratio_clean <- clean_ea_data(files_ratio) %>%
  filter(!str_detect(sample_no, "SRM")) %>%
  mutate(sample_no = as.numeric(sample_no))

# Flag outliers
ratio_flags <- flag_outliers(cn_ratio_clean, "ratio")

# Join with percentage data
cn_clean_all <- percent_flags %>%
  filter(!str_detect(sample_no, "SRM")) %>%
  mutate(sample_no = as.numeric(sample_no)) %>%
  left_join(ratio_flags)

# Map samples and results to master list of treatments
all_treatments <- read_csv("output/2022/jar_assignments/master_list.csv")

mapped_all <- cn_clean_all %>%
  left_join(all_treatments)
# Create sample summary for ratios, %N and %C
samp_sum_all <- mapped_all %>%
  group_by(sample_no, cc_treatment, drying_treatment, pre_post_wet) %>%
  summarize(samp_cn_med = median(c_n_ratio),
            samp_n_med = median(n_per),
            samp_c_med = median(c_per))

# Save out sample summary to use for correlation tests
write_csv(samp_sum_all, "data/cleaned_data/EA/samp_sum_all.csv")

#### COMPARE %N BETWEEN CC IN CW ####
# See how adding cc affects %N as it decomposes with cw samples
# ie initial + cw samples only
# Calculate medians per sample
n_cw_samp_sum_all <- samp_sum_all %>%
  filter(pre_post_wet == "cw" |
           pre_post_wet == "initial")
# Calculate medians per treatment level
n_cw_treat_sum_all <- n_cw_samp_sum_all %>%
  group_by(cc_treatment, drying_treatment) %>%
  summarize(treat_median = median(samp_median),
            treat_iqr = IQR(samp_median)) %>%
  arrange(factor(drying_treatment,
                 levels = c("initial", "one_wk", "two_wk", "four_wk")))

# Create facet labels
facet_drying_labels <- as_labeller(c("one_wk" = "One Week",
                                     "two_wk" = "Two Weeks",
                                     "four_wk" = "Four Weeks",
                                     "initial" = "Initial"))
# Plot
n_cw_plot_all <- n_cw_samp_sum_all %>%
  ggplot(aes(x = factor(cc_treatment, levels = c("no_cc", "w_cc")),
             y = samp_median,
             fill = cc_treatment,
             color = cc_treatment)) +
  geom_boxplot() +
  facet_grid(~ factor(drying_treatment,
                      levels = c("initial", "one_wk", "two_wk", "four_wk")),
             labeller = facet_drying_labels)  +
  scale_fill_manual(name = NULL, limits = c("no_cc", "w_cc"),
                    values = c("#16B4FF", "#34980D"),
                    labels = c("No Cover Crop", "With Cover Crop")) +
  scale_color_manual(name = NULL, limits = c("no_cc", "w_cc"),
                     values = c("#097CB2", "#195004"),
                     labels = c("No Cover Crop", "With Cover Crop")) +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom") +
  labs(y = "% Nitrogen",
       title = "Nitrogen in Consistently Moist Soils")
ggsave(n_cw_plot_all, filename = "output/2022/ea_plots/nper_cc_cw.png",
       width = 14, height = 8, units = "in")

# Calculate stats on effect of cc on %N over time in cw
n_cw_stats_all <- n_cw_samp_sum_all %>%
  kruskal.test(data = ., samp_median ~ cc_treatment)
# Calculate stats on effect of drying on %N over time in cw
n_cw_samp_sum_all %>%
  kruskal.test(data = ., samp_median ~ drying_treatment)

# Calculate stats for each week to see effect between w_cc and no_cc
cw_wk_stats <- data.frame("drying_treatment" = character(),
                          "p_value"  = numeric(),
                          "chi_sq" = numeric())
weeks <- c("initial", "one_wk", "two_wk", "four_wk")
for(a in weeks) {
  cw_stats <- n_cw_samp_sum_all %>%
    filter(drying_treatment == a) %>%
    kruskal.test(data = ., samp_median ~ cc_treatment)
  new <- data.frame("drying_treatment" = a,
                    "p_value" = cw_stats$p.value,
                    "chi_sq" = cw_stats$statistic)
  cw_wk_stats <- rbind(cw_wk_stats, new)
}


#### COMPARE C:N BETWEEN CC IN CW ####
# Effect of cc on C:N in cw (Objective 2)
# Summarize cw only data
ratio_cw_treat_sum <- samp_sum_all %>%
  filter(pre_post_wet == "cw" |
           pre_post_wet == "initial") %>%
  group_by(cc_treatment, drying_treatment) %>%
  summarize(treat_cn_med = median(samp_cn_med),
            treat_cn_iqr = IQR(samp_cn_med)) %>%
  arrange(factor(drying_treatment,
                 levels = c("initial", "one_wk", "two_wk", "four_wk")))
# Plot across time
ratio_cw_plot <- samp_sum_all %>%
  filter(pre_post_wet == "cw" |
           pre_post_wet == "initial") %>%
  ggplot(aes(x = cc_treatment,
             y = samp_cn_med,
             fill = cc_treatment,
             color = cc_treatment)) +
  geom_boxplot() +
  facet_grid(~ factor(drying_treatment,
                      levels = c("initial", "one_wk", "two_wk", "four_wk")),
             labeller = facet_drying_labels) +
  scale_fill_manual(name = NULL, limits = c("no_cc", "w_cc"),
                    values = c("#16B4FF", "#34980D"),
                    labels = c("No Cover Crop", "With Cover Crop")) +
  scale_color_manual(name = NULL, limits = c("no_cc", "w_cc"),
                     values = c("#097CB2", "#195004"),
                     labels = c("No Cover Crop", "With Cover Crop")) +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom") +
  labs(y = "C:N Ratios",
       title = "C:N of Consistently Moist Soils")
ggsave(ratio_cw_plot, filename = "output/2022/ea_plots/ratio_cn_cw.png",
       width = 12, height = 8, units = "in")

# Calculate stats per week to see effect of cc treatment
cw_cn_wk_stats <- data.frame("drying_treatment" = character(),
                          "p_value"  = numeric(),
                          "chi_sq" = numeric())
for(a in weeks) {
  cw_cn_stats <- samp_sum_all %>%
    filter(pre_post_wet == "cw" |
             pre_post_wet == "initial") %>%
    filter(drying_treatment == a) %>%
    kruskal.test(data = ., samp_cn_med ~ cc_treatment)
  new <- data.frame("drying_treatment" = a,
                    "p_value" = cw_cn_stats$p.value,
                    "chi_sq" = cw_cn_stats$statistic)
  cw_cn_wk_stats <- rbind(cw_cn_wk_stats, new)
}


####  COMPARE %N BETWEEN CC IN DRYING ####
# Examine the effects of cc on %N by comparing w_cc and no_cc when dried
# ie initial + pre-wet samples only
# Calculate medians per sample
n_dry_samp_sum_all <- samp_sum_all %>%
  filter(pre_post_wet == "pre" |
           pre_post_wet == "initial")
# Calculate medians per treatment level
n_dry_treat_sum_all <- n_dry_samp_sum_all %>%
  group_by(cc_treatment, drying_treatment) %>%
  summarize(treat_median = median(samp_median),
            treat_iqr = IQR(samp_median)) %>%
  arrange(factor(drying_treatment,
                 levels = c("initial", "one_wk", "two_wk", "four_wk")))

# Plot
n_dry_plot_all <- n_dry_samp_sum_all %>%
  ggplot(aes(x = factor(cc_treatment, levels = c("no_cc", "w_cc")),
             y = samp_n_med,
             fill = cc_treatment,
             color = cc_treatment)) +
  geom_boxplot() +
  facet_grid(~ factor(drying_treatment,
                      levels = c("initial", "one_wk", "two_wk", "four_wk")),
             labeller = facet_drying_labels)  +
  scale_fill_manual(name = NULL, limits = c("no_cc", "w_cc"),
                    values = c("#16B4FF", "#34980D"),
                    labels = c("No Cover Crop", "With Cover Crop")) +
  scale_color_manual(name = NULL, limits = c("no_cc", "w_cc"),
                     values = c("#097CB2", "#195004"),
                     labels = c("No Cover Crop", "With Cover Crop")) +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom") +
  labs(y = "% Nitrogen",
       title = "Nitrogen in Drying Soils")
ggsave(n_dry_plot_all, filename = "output/2022/ea_plots/nper_cc_drying.png",
       width = 14, height = 8, units = "in")

# Calculate stats on effect of cc on %N as soils dry
n_dry_stats_all <- n_dry_samp_sum_all %>%
  kruskal.test(data = ., samp_median ~ cc_treatment)
# Calculate stats on effect of drying on %N as soils dry
n_dry_samp_sum_all %>%
  kruskal.test(data = ., samp_median ~ drying_treatment)

# Calculate stats for each week between w_cc and no_cc
dry_wk_stats <- data.frame("drying_treatment" = character(),
                          "p_value"  = numeric(),
                          "chi_sq" = numeric())
weeks <- c("initial", "one_wk", "two_wk", "four_wk")
for(a in weeks) {
  dry_stats <- n_dry_samp_sum_all %>%
    filter(drying_treatment == a) %>%
    kruskal.test(data = ., samp_median ~ cc_treatment)
  new <- data.frame("drying_treatment" = a,
                    "p_value" = dry_stats$p.value,
                    "chi_sq" = dry_stats$statistic)
  dry_wk_stats <- rbind(dry_wk_stats, new)
}

#### COMPARE C:N IN DRYING ####
# Effect of cc on C:N across drying (Objective 2)
# Summarize cw only data
ratio_dry_treat_sum <- samp_sum_all %>%
  filter(pre_post_wet == "pre" |
           pre_post_wet == "initial") %>%
  group_by(cc_treatment, drying_treatment) %>%
  summarize(treat_cn_med = median(samp_cn_med),
            treat_cn_iqr = IQR(samp_cn_med)) %>%
  arrange(factor(drying_treatment,
                 levels = c("initial", "one_wk", "two_wk", "four_wk")))
# Plot across time
ratio_dry_plot <- samp_sum_all %>%
  filter(pre_post_wet == "pre" |
           pre_post_wet == "initial") %>%
  ggplot(aes(x = cc_treatment,
             y = samp_cn_med,
             fill = cc_treatment,
             color = cc_treatment)) +
  geom_boxplot() +
  facet_grid(~ factor(drying_treatment,
                      levels = c("initial", "one_wk", "two_wk", "four_wk")),
             labeller = facet_drying_labels) +
  scale_fill_manual(name = NULL, limits = c("no_cc", "w_cc"),
                    values = c("#16B4FF", "#34980D"),
                    labels = c("No Cover Crop", "With Cover Crop")) +
  scale_color_manual(name = NULL, limits = c("no_cc", "w_cc"),
                     values = c("#097CB2", "#195004"),
                     labels = c("No Cover Crop", "With Cover Crop")) +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom") +
  labs(y = "C:N Ratios",
       title = "C:N in Drying Soils")
ggsave(ratio_dry_plot, filename = "output/2022/ea_plots/ratio_cn_drying.png",
       width = 12, height = 8, units = "in")

# Calculate stats per week on effect of cc treatment
dry_cn_wk_stats <- data.frame("drying_treatment" = character(),
                           "p_value"  = numeric(),
                           "chi_sq" = numeric())
for(a in weeks) {
  dry_cn_stats <- samp_sum_all %>%
    filter(drying_treatment == a) %>%
    kruskal.test(data = ., samp_cn_med ~ cc_treatment)
  new <- data.frame("drying_treatment" = a,
                    "p_value" = dry_cn_stats$p.value,
                    "chi_sq" = dry_cn_stats$statistic)
  dry_cn_wk_stats <- rbind(dry_cn_wk_stats, new)
}


### COMPARE C:N BEFORE/AFTER REWETTING
# Effect of cc treatment on C:N before / after rewetting (Objective 3)

# Summarize treatments across all samples
ratio_treat_sum <- samp_sum_all %>%
  group_by(cc_treatment, drying_treatment, pre_post_wet) %>%
  summarize(treat_cn_med = median(samp_cn_med),
            treat_cn_iqr = IQR(samp_cn_med))
# Filter by pre/post only to create summary
ratio_prepost_sum <- ratio_treat_sum %>%
  filter(pre_post_wet == "pre" |
           pre_post_wet == "post") %>%
  arrange(factor(cc_treatment, levels = c("no_cc", "w_cc")),
          factor(drying_treatment, levels = c("one_wk", "two_wk", "four_wk")),
          factor(pre_post_wet, levels = c("pre", "post")))

# Plot pre/post between cc at every week
drying_treatments <- list("one_wk", "two_wk", "four_wk")
for (a in drying_treatments) {
  wk_title <- case_when(a == "one_wk" ~ "One Week",
                        a == "two_wk" ~ "Two Weeks",
                        a == "four_wk" ~ "Four Weeks")
  wk_plot <- ratio_samp_sum %>%
    filter(pre_post_wet == "pre" |
             pre_post_wet == "post") %>%
    filter(drying_treatment == a) %>%
    ggplot(aes(x = factor(pre_post_wet, levels = c("pre", "post")),
               y = samp_cn_med,
               fill = cc_treatment,
               color = cc_treatment)) +
    geom_boxplot() +
    scale_fill_manual(name = NULL, limits = c("no_cc", "w_cc"),
                      values = c("#16B4FF", "#34980D"),
                      labels = c("No Cover Crop", "With Cover Crop")) +
    scale_color_manual(name = NULL, limits = c("no_cc", "w_cc"),
                       values = c("#097CB2", "#195004"),
                       labels = c("No Cover Crop", "With Cover Crop")) +
    theme(legend.position = "bottom",
          axis.title.x = element_blank()) +
    labs(title = paste0("C:N in Rewet Soils Dried for ", wk_title),
         y = "C:N Ratio") +
    scale_x_discrete(labels = c("Pre-Wet", "Post-Wet"))
  assign(paste0("plot_", a), wk_plot)
}

# Create faceted plot where drying_treatment are the facets showing pre/post
all_ratio_plot <- samp_sum_all %>%
  filter(pre_post_wet == "pre" |
           pre_post_wet == "post") %>%
  ggplot(aes(x = factor(pre_post_wet, levels = c("pre", "post")),
         y = (samp_cn_med),
         fill = cc_treatment,
         color = cc_treatment)) +
  geom_boxplot() +
  facet_wrap(~ factor(drying_treatment,
                      levels = c("one_wk", "two_wk", "four_wk")),
             labeller = facet_drying_labels)  +
  scale_fill_manual(name = NULL, limits = c("no_cc", "w_cc"),
                    values = c("#16B4FF", "#34980D"),
                    labels = c("No Cover Crop", "With Cover Crop")) +
  scale_color_manual(name = NULL, limits = c("no_cc", "w_cc"),
                     values = c("#097CB2", "#195004"),
                     labels = c("No Cover Crop", "With Cover Crop")) +
  theme(legend.position = "bottom",
        axis.title.x = element_blank()) +
  scale_x_discrete(labels = c("Pre-Wet", "Post-Wet")) +
  labs(y = "C:N Ratios",
       title = "C:N Ratios in Pre- and Post-Wet Soils")
ggsave(all_ratio_plot, filename = "output/2022/ea_plots/cn_ratio_all_plot.png",
       width = 14, height = 8, units = "in")

# Stats for each week to see effect of pre/post wet per cc treatment on ratios
cc_treatments <- list("no_cc", "w_cc")
all_cc_cn_stats <- data.frame("drying_treatment" = as.character(),
                        "cc_treatment" = as.character(),
                        "p_value" = as.numeric(),
                        "chi_sq" = as.numeric())
for (a in drying_treatments) {
  for (b in cc_treatments) {
    wk_stats <- ratio_samp_sum %>%
      filter(cc_treatment == b) %>%
      filter(drying_treatment == a) %>%
      filter(pre_post_wet == "pre" |
               pre_post_wet == "post") %>%
      kruskal.test(data = ., samp_cn_med ~ pre_post_wet)
    drying_treat <- a
    p_val <- wk_stats$p.value
    chi_sq <- wk_stats$statistic
    new_data <- list("drying_treatment" = drying_treat,
                     "cc_treatment" = b,
                     "p_value" = p_val,
                     "chi_sq" = chi_sq)
    all_cc_cn_stats <- rbind(all_cc_cn_stats, new_data)
  }
}


### COMPARE %C BEFORE/AFTER REWETTING
# Effect of cc treatment (Objective 3)

# Summarize treatments across all samples
c_treat_sum <- samp_sum_all %>%
  group_by(cc_treatment, drying_treatment, pre_post_wet) %>%
  summarize(treat_c_med = median(samp_c_med),
            treat_c_iqr = IQR(samp_c_med))
# Filter by pre/post only to create summary
c_prepost_sum <- c_treat_sum %>%
  filter(pre_post_wet == "pre" |
           pre_post_wet == "post") %>%
  arrange(factor(cc_treatment, levels = c("no_cc", "w_cc")),
          factor(drying_treatment, levels = c("one_wk", "two_wk", "four_wk")),
          factor(pre_post_wet, levels = c("pre", "post")))

# Create faceted plot where drying_treatment are the facets showing pre/post
all_c_plot <- samp_sum_all %>%
  filter(pre_post_wet == "pre" |
           pre_post_wet == "post") %>%
  ggplot(aes(x = factor(pre_post_wet, levels = c("pre", "post")),
             y = (samp_c_med),
             fill = cc_treatment,
             color = cc_treatment)) +
  geom_boxplot() +
  facet_wrap(~ factor(drying_treatment,
                      levels = c("one_wk", "two_wk", "four_wk")),
             labeller = facet_drying_labels)  +
  scale_fill_manual(name = NULL, limits = c("no_cc", "w_cc"),
                    values = c("#16B4FF", "#34980D"),
                    labels = c("No Cover Crop", "With Cover Crop")) +
  scale_color_manual(name = NULL, limits = c("no_cc", "w_cc"),
                     values = c("#097CB2", "#195004"),
                     labels = c("No Cover Crop", "With Cover Crop")) +
  theme(legend.position = "bottom",
        axis.title.x = element_blank()) +
  scale_x_discrete(labels = c("Pre-Wet", "Post-Wet")) +
  labs(y = "% Carbon",
       title = "Carbon in Pre- and Post-Wet Soils")
ggsave(all_c_plot, filename = "output/2022/ea_plots/c_all_plot.png",
       width = 14, height = 8, units = "in")

# Stats for each week to see effect of pre/post wet per cc treatment on ratios
cc_treatments <- list("no_cc", "w_cc")
all_cc_c_stats <- data.frame("drying_treatment" = as.character(),
                           "cc_treatment" = as.character(),
                           "p_value" = as.numeric(),
                           "chi_sq" = as.numeric())
for (a in drying_treatments) {
  for (b in cc_treatments) {
    wk_stats <- samp_sum_all %>%
      filter(cc_treatment == b) %>%
      filter(drying_treatment == a) %>%
      filter(pre_post_wet == "pre" |
               pre_post_wet == "post") %>%
      kruskal.test(data = ., samp_c_med ~ pre_post_wet)
    drying_treat <- a
    p_val <- wk_stats$p.value
    chi_sq <- wk_stats$statistic
    new_data <- list("drying_treatment" = drying_treat,
                     "cc_treatment" = b,
                     "p_value" = p_val,
                     "chi_sq" = chi_sq)
    all_cc_c_stats <- rbind(all_cc_c_stats, new_data)
  }
}
