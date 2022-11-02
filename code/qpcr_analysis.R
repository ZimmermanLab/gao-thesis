# This script analyses data from the qPCR assays.

# Sarah Gao
# July 27th, 2022
# hellosarahgao@gmail.com

library("tidyr")
library("dplyr")
library("readr")
library("stringi")
library("stringr")
library("ggplot2")
library("fs")

# Source data cleaning function
source("code/functions/qpcr_functions/clean_qpcr_data.R")

# Get clean fungal data
clean_fungal <- clean_qpcr_data("data/raw_data/qPCR/", "fungal")
# Get clean bacterial data
clean_bacterial <- clean_qpcr_data("data/raw_data/qPCR/", "bacterial")

# Bring in treatment mapping
all_treatments <- read_csv("output/2022/jar_assignments/master_list.csv")

# Flag outliers from the datasets
source("code/functions/qpcr_functions/flag_outliers.R")
bacterial_outlier_flags <- flag_outliers(clean_bacterial)
fungal_outlier_flags <- flag_outliers(clean_fungal)

# Find proportional starting concentrations using 2^-Cq
source("code/functions/qpcr_functions/calculate_prop_concentrations.R")
bact_concentrations <- calc_prop_conc(bacterial_outlier_flags)
fung_concentrations <- calc_prop_conc(fungal_outlier_flags)

# Bring in dried soil weights and Qubit concentrations
dried_wts_qubit <- read_csv("data/raw_data/qPCR/2022_DNA.csv") %>%
  select(sample_no, extraction_soil_wt_mg,
         qubit_concentration, dry_soil_only) %>%
  filter(sample_no != "bacteria-control") %>%
  filter(!(is.na(dry_soil_only)))
dried_wts_qubit$sample_no <- as.double(dried_wts_qubit$sample_no)
dried_wts_qubit$qubit_concentration <-as.double(
  dried_wts_qubit$qubit_concentration)

# Normalize the proportional concentrations
source("code/functions/qpcr_functions/normalize_prop_concentration.R")
bact_norm_all <- norm_conc(bact_concentrations, dried_wts_qubit) %>%
  rename(conc_norm_bact = prop_conc_norm,
         cq_bact = cq,
         outlier_flag_bact = outlier_flag)
fung_norm_all <- norm_conc(fung_concentrations, dried_wts_qubit) %>%
  rename(conc_norm_fung = prop_conc_norm,
         cq_fung = cq,
         outlier_flag_fung = outlier_flag)

### FUNGAL:BACTERIAL RATIOS ###
# Find median per sample for bacterial and fungal separately
samp_rep_fung <- fung_norm_all %>%
  group_by(sample_no) %>%
  summarize(samp_rep_med_fung = median(conc_norm_fung))
samp_rep_bact <- bact_norm_all %>%
  group_by(sample_no) %>%
  summarise(samp_rep_med_bact = median(conc_norm_bact))

# Find median per sample (all included) and join to treatments
samp_all <- samp_rep_bact %>%
  left_join(samp_rep_fung) %>%
  left_join(all_treatments) %>%
  group_by(sample_no, cc_treatment, drying_treatment, pre_post_wet) %>%
  summarize(samp_med_bact = median(samp_rep_med_bact),
            samp_med_fung = median(samp_rep_med_fung))

# Find ratios per sample
# Note that I did this at the sample level vs the tech rep level since fungal
# and bacterial tech reps cannot be mapped to each other
samp_ratio_all <- samp_all %>%
  mutate(samp_ratio = samp_med_fung / samp_med_bact) %>%
  # Removes water only samples that had bacteria but no fungi
  filter(!is.na(samp_ratio))

# NO CC ONLY
samp_ratio_nocc <- samp_ratio_all %>%
  filter(cc_treatment == "no_cc") %>%
  filter(pre_post_wet == "pre"|
           pre_post_wet == "post")
# Calculate per treatment medians + IQs
treat_ratio_nocc_sum <- samp_ratio_nocc %>%
  group_by(cc_treatment, drying_treatment, pre_post_wet) %>%
  summarize(treat_median_ratio = median(samp_ratio),
            treat_iqr = IQR(samp_ratio)) %>%
  arrange(drying_treatment == "one_wk")

# Calculate total statistics of rewetting in no cc
ratio_stats_rewet_nocc <- samp_ratio_nocc %>%
  kruskal.test(data = ., samp_ratio ~ pre_post_wet)
# Calculate stats of drying in pre only
ratio_stats_drying_nocc <- samp_ratio_nocc %>%
  filter(pre_post_wet == "pre") %>%
  kruskal.test(data = ., samp_ratio ~ drying_treatment)

# Calculate per week differences in pre-post
ratio_nocc_one_wk_stats <- samp_ratio_nocc %>%
  filter(drying_treatment == "one_wk") %>%
  kruskal.test(data = ., samp_ratio ~ pre_post_wet)
ratio_nocc_two_wk_stats <- samp_ratio_nocc %>%
  filter(drying_treatment == "two_wk") %>%
  kruskal.test(data = ., samp_ratio ~ pre_post_wet)
ratio_nocc_four_wk_stats <- samp_ratio_nocc %>%
  filter(drying_treatment == "four_wk") %>%
  kruskal.test(data = ., samp_ratio ~ pre_post_wet)

# Plot ratios
# Set facet labels
facet_drying_labels <- as_labeller(c("one_wk" = "One Week",
                                     "two_wk" = "Two Weeks",
                                     "four_wk" = "Four Weeks"))
# No cc
ratio_nocc_plot <- samp_ratio_nocc %>%
  ggplot(aes(x = factor(pre_post_wet, levels = c("pre", "post")),
             y = samp_ratio,
             fill = pre_post_wet,
             color = pre_post_wet)) +
  geom_boxplot() +
  facet_wrap(~ factor(drying_treatment,
                      levels = c("one_wk", "two_wk", "four_wk")),
             labeller = facet_drying_labels)  +
  scale_fill_manual(name = NULL, limits = c("pre", "post"),
                    values = c("#16B4FF", "#34980D"),
                    labels = c("Pre-Wet", "Post-Wet")) +
  scale_color_manual(name = NULL, limits = c("pre", "post"),
                     values = c("#097CB2", "#195004"),
                     labels = c("Pre-Wet", "Post-Wet")) +
  scale_x_discrete(labels = c("Pre-Wet", "Post-Wet")) +
  scale_y_continuous(trans = "log10") +
  theme(legend.position = "none") +
  labs(x = element_blank(),
       y = "Ratios (log scale)",
       title = "Fungal:Bacterial Ratios in Soils Without Cover Crop")
ggsave(ratio_nocc_plot,
       filename = "output/2022/qpcr_plots/ratios_all_nocc.png",
       width = 10, height = 8, units = "in")


# W CC ONLY
samp_ratio_wcc <- samp_ratio_all %>%
  filter(cc_treatment == "w_cc") %>%
  filter(pre_post_wet == "pre"|
           pre_post_wet == "post")
# Calculate per treatment medians + IQs
treat_ratio_wcc_sum <- samp_ratio_wcc %>%
  group_by(cc_treatment, drying_treatment, pre_post_wet) %>%
  summarize(treat_median_ratio = median(samp_ratio),
            treat_iqr = IQR(samp_ratio)) %>%
  arrange(drying_treatment == "one_wk")

# Calculate total statistics of rewetting in w cc
ratio_stats_rewet_wcc <- samp_ratio_wcc %>%
  kruskal.test(data = ., samp_ratio ~ pre_post_wet)
# Calculate stats of drying in pre only
ratio_stats_drying_wcc <- samp_ratio_wcc %>%
  filter(pre_post_wet == "pre") %>%
  kruskal.test(data = ., samp_ratio ~ drying_treatment)

# Calculate per week differences in pre-post
ratio_wcc_one_wk_stats <- samp_ratio_wcc %>%
  filter(drying_treatment == "one_wk") %>%
  kruskal.test(data = ., samp_ratio ~ pre_post_wet)
ratio_wcc_two_wk_stats <- samp_ratio_wcc %>%
  filter(drying_treatment == "two_wk") %>%
  kruskal.test(data = ., samp_ratio ~ pre_post_wet)
ratio_wcc_four_wk_stats <- samp_ratio_wcc %>%
  filter(drying_treatment == "four_wk") %>%
  kruskal.test(data = ., samp_ratio ~ pre_post_wet)

# Plot w cc
ratio_wcc_plot <- samp_ratio_wcc %>%
  ggplot(aes(x = factor(pre_post_wet, levels = c("pre", "post")),
             y = samp_ratio,
             fill = pre_post_wet,
             color = pre_post_wet)) +
  geom_boxplot() +
  facet_wrap(~ factor(drying_treatment,
                      levels = c("one_wk", "two_wk", "four_wk")),
             labeller = facet_drying_labels)  +
  scale_fill_manual(name = NULL, limits = c("pre", "post"),
                    values = c("#16B4FF", "#34980D"),
                    labels = c("Pre-Wet", "Post-Wet")) +
  scale_color_manual(name = NULL, limits = c("pre", "post"),
                     values = c("#097CB2", "#195004"),
                     labels = c("Pre-Wet", "Post-Wet")) +
  scale_x_discrete(labels = c("Pre-Wet", "Post-Wet")) +
  scale_y_continuous(trans = "log10") +
  theme(legend.position = "none") +
  labs(x = element_blank(),
       y = "Ratios (log scale)",
       title = "Fungal:Bacterial Ratios in Soils With Cover Crop")
ggsave(ratio_wcc_plot,
       filename = "output/2022/qpcr_plots/ratios_all_wcc.png",
       width = 10, height = 8, units = "in")

# NO OUTLIERS
# Find median per sample (no outliers)
samp_nomod <- samp_rep_ratio_all %>%
  filter(is.na(outlier_flag_bact) |
           is.na(outlier_flag_fung)) %>%
  group_by(sample_no, cc_treatment, drying_treatment, pre_post_wet) %>%
  summarize(samp_med_bact = median(conc_norm_bact),
            samp_med_fung = median(conc_norm_fung))

# Find ratios per sample
samp_ratio_nomod <- samp_nomod %>%
  mutate(samp_ratio = samp_med_fung / samp_med_bact) %>%
  # Removes water only samples that had bacteria but no fungi
  filter(!is.na(samp_ratio))

# Plot
samp_ratio_nomod_plot <- samp_ratio_nomod %>%
  filter(cc_treatment == "w_cc") %>%
  filter(pre_post_wet == "pre" |
           pre_post_wet == "post") %>%
  ggplot(aes(x = factor(pre_post_wet, levels = c("pre", "post")),
             y = samp_ratio,
             fill = pre_post_wet,
             color = pre_post_wet)) +
  geom_boxplot() +
  facet_wrap(~ factor(drying_treatment,
                      levels = c("one_wk", "two_wk", "four_wk")),
             labeller = facet_drying_labels)  +
  scale_fill_manual(name = NULL, limits = c("pre", "post"),
                    values = c("#16B4FF", "#34980D"),
                    labels = c("Pre-Wet", "Post-Wet")) +
  scale_color_manual(name = NULL, limits = c("pre", "post"),
                     values = c("#097CB2", "#195004"),
                     labels = c("Pre-Wet", "Post-Wet")) +
  scale_y_continuous(trans = "log10") +
  scale_x_discrete(labels = c("Pre-Wet", "Post-Wet")) +
  theme(legend.position = "none")
## REMOVING ALL OUTLIERS FROM TECH REPS DOES NOT CHANGE EITHER
# W CC OR W/O CC


#### SEPARATE BACTERIAL + FUNGAL ANALYSES ####

# Map samples to all treatment conditions
bact_treatment_all <- bact_norm_all %>%
  left_join(all_treatments)
fung_treatment_all <- fung_norm_all %>%
  left_join(all_treatments)

# Without tech replicate extreme outliers (> 3x IQR)
bact_treatment_no_ext <- bact_treatment_all %>%
  filter(!(outlier_flag == "extreme") |
           is.na(outlier_flag)) %>%
  select(-c(outlier_flag))
fung_treatment_no_ext <- fung_treatment_all %>%
  filter(!(outlier_flag == "extreme") |
           is.na(outlier_flag)) %>%
  select(-c(outlier_flag))
# Without tech replicate extreme and moderate outliers (> 1.5x IQR)
fung_treatment_no_mod <- fung_treatment_all %>%
  filter(is.na(outlier_flag)) %>%
  select(-c(outlier_flag))
bact_treatment_no_mod <- bact_treatment_all %>%
  filter(is.na(outlier_flag)) %>%
  select(-c(outlier_flag))

# Make plot and run Kruskal-Wallis test to see effect of rewetting on
# DNA quantities
source("code/functions/qpcr_functions/analyze_plot_dna.R")
# in samples without cc
bact_rewet_no_cc <- analyze_plot_dna(bact_treatment_all, "no_cc")
# Run test at every time point to compare differences before and after
source("code/functions/qpcr_functions/compare_rewet_each_wk.R")
bact_wk_no_cc_all <- compare_rewet_time(bact_treatment_all, "no_cc")
# Fungal
fung_rewet_no_cc <- analyze_plot_dna(fung_treatment_all, "no_cc")
fung_wk_no_cc_all <- compare_rewet_time(fung_treatment_all, "no_cc")


# Make plot and run Kruskal-Wallis test to see effect of rewetting on
# bacterial DNA quantities in samples with cc
bact_rewet_w_cc <- analyze_plot_dna(bact_treatment_all, "w_cc")
bact_wk_w_cc_all <- compare_rewet_time(bact_treatment_all, "w_cc")

# Make plot and run Kruskal-Wallis test to see effect of rewetting on
# fungal DNA quantities in samples with cc
fung_rewet_w_cc <- analyze_plot_dna(fung_treatment_all, "w_cc")
fung_wk_w_cc_all <- compare_rewet_time(fung_treatment_all, "w_cc")

#### Checking outliers ####

# Plot + analyse with no extreme outliers
bact_rewet_no_cc_noext <- analyze_plot_dna(bact_treatment_no_ext, "no_cc")
bact_wk_no_cc_noext <- compare_rewet_time(bact_treatment_no_ext, "no_cc")

fung_rewet_no_cc_noext <- analyze_plot_dna(fung_treatment_no_ext, "no_cc")
fung_wk_no_cc_noext <- compare_rewet_time(fung_treatment_no_ext, "no_cc")

bact_rewet_w_cc_noext <- analyze_plot_dna(bact_treatment_no_ext, "w_cc")
bact_wk_w_cc_noext <- compare_rewet_time(bact_treatment_no_ext, "w_cc")

fung_rewet_w_cc_noext <- analyze_plot_dna(fung_treatment_no_ext, "w_cc")
fung_wk_w_cc_noext <- compare_rewet_time(fung_treatment_no_ext, "w_cc")

# Plot + analyse with no extreme or moderate outliers
bact_rewet_no_cc_nomod <- analyze_plot_dna(bact_treatment_no_mod, "no_cc")
bact_wk_no_cc_nomod <- compare_rewet_time(bact_treatment_no_mod, "no_cc")

fung_rewet_no_cc_nomod <- analyze_plot_dna(fung_treatment_no_mod, "no_cc")
fung_wk_no_cc_nomod <- compare_rewet_time(fung_treatment_no_mod, "no_cc")

bact_rewet_w_cc_nomod <- analyze_plot_dna(bact_treatment_no_mod, "w_cc")
bact_wk_w_cc_noext <- compare_rewet_time(bact_treatment_no_mod, "w_cc")

fung_rewet_w_cc_nomod <- analyze_plot_dna(fung_treatment_no_mod, "w_cc")
fung_wk_w_cc_nomod <- compare_rewet_time(fung_treatment_no_mod, "w_cc")
