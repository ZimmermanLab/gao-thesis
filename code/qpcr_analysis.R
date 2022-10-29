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
clean_fungal <- clean_qpcr_data(files_fungal, "fungal")

# Get clean bacterial data
clean_bacterial <- clean_qpcr_data(files_bacterial_full, "bacterial")

# Filter out NA values and store them separately
na_only_fungal <- clean_fungal %>%
  filter(cq == "NaN") %>%
  select(sample_no, rep_no, cq)
na_only_bacterial <- clean_bacterial %>%
  filter(cq == "NaN") %>%
  select(sample_no, rep_no, cq)

# Bring in treatment mapping and get water only soil numbers
all_treatments <- read_csv("output/2022/jar_assignments/master_list.csv")
no_soil <- all_treatments %>%
  filter(pre_post_wet == "no_soil")

# Filter water only samples from bacterial dataset
bacterial_no_h2o <- clean_bacterial_no_na %>%
  filter(!(sample_no %in% no_soil$sample_no))

# Flag outliers from the datasets
source("code/functions/qpcr_functions/flag_outliers.R")
bacterial_outlier_flags <- flag_outliers(bacterial_no_h2o)
fungal_outlier_flags <- flag_outliers(clean_fungal_no_na)

# Use all data (includes all outliers)
fungal_all <- fungal_outlier_flags %>%
  select(-c(outlier_flag))
bacterial_all <- bacterial_outlier_flags %>%
  select(-c(outlier_flag))
# Filter out extreme outliers (> 3x IQR)
fungal_no_ext <- fungal_outlier_flags %>%
  filter(!(outlier_flag == "extreme") |
           is.na(outlier_flag)) %>%
  select(-c(outlier_flag))
bacterial_no_ext <- bacterial_outlier_flags %>%
  filter(!(outlier_flag == "extreme") |
           is.na(outlier_flag)) %>%
  select(-c(outlier_flag))
# Filter out extreme and moderate outliers (> 1.5x IQR)
fungal_no_mod <- fungal_outlier_flags %>%
  filter(is.na(outlier_flag)) %>%
  select(-c(outlier_flag))
bacterial_no_mod <- bacterial_outlier_flags %>%
  filter(is.na(outlier_flag)) %>%
  select(-c(outlier_flag))

# Find proportional starting concentrations using 2^-Cq
source("code/functions/qpcr_functions/calculate_prop_concentrations.R")
bact_concentrations_all <- calc_prop_conc(bacterial_all)
bact_concentrations_no_mod <- calc_prop_conc(bacterial_no_mod)
fung_concentrations_all <- calc_prop_conc(fungal_all)

# Bring in dried soil weights and Qubit concentrations
dried_wts_qubit <- read_csv("data/raw_data/qPCR/2022_DNA.csv") %>%
  select(sample_no, extraction_soil_wt_mg,
         qubit_concentration, dry_soil_only) %>%
  filter(sample_no != "bacteria-control") %>%
  filter(!(is.na(dry_soil_only)))
dried_wts_qubit$sample_no <- as.double(dried_wts_qubit$sample_no)
dried_wts_qubit$qubit_concentration <-as.double(
  dried_wts_qubit$qubit_concentration)

# Normalize the proportional concentrations and calculate
# fungal:bacterial ratios
source("code/functions/qpcr_functions/normalize_prop_concentration.R")
bact_norm_all <- norm_conc(bact_concentrations_all, dried_wts_qubit)
bact_norm_no_mod <- norm_conc(bact_concentrations_no_mod, dried_wts_qubit)
fung_norm_all <- norm_conc(fung_concentrations_all, dried_wts_qubit)
fung_norm_no_mod <- norm_conc(fung_concentrations_all, dried_wts_qubit)

# Map samples to all treatment conditions
bact_treatment_all <- bact_norm_all %>%
  left_join(all_treatments)
bact_treatment_no_mod <- bact_norm_no_mod %>%
  left_join(all_treatments)
fung_treatment_all <- fung_norm_all %>%
  left_join(all_treatments)
fung_treatment_no_mod <- fung_norm_no_mod %>%
  left_join(all_treatments)

# Make plot and run Kruskal-Wallis test to see effect of rewetting on
# bacterial DNA quantities in samples without cc
source("code/functions/qpcr_functions/analyze_plot_dna.R")
bact_rewet_no_cc <- analyze_plot_dna(bact_treatment_all, "no_cc")
bact_rewet_stats_no_cc <- bact_rewet_no_cc[1]
bact_rewet_plot_no_cc <- bact_rewet_no_cc[2]
bact_rewet_med_no_cc <- bact_rewet_no_cc[3]

# Make plot and run Kruskal-Wallis test to see effect of rewetting on
# fungal DNA quantities in samples without cc
fung_rewet_no_cc <- analyze_plot_dna(fung_treatment_all, "no_cc")
fung_rewet_stats_no_cc <- fung_rewet_no_cc[1]
fung_rewet_plot_no_cc <- fung_rewet_no_cc[2]
fung_rewet_med_no_cc <- fung_rewet_no_cc[3]

# Run test at every time point to compare differences before and after
# Bacteria no cc
bact_no_cc_one_wk <- bact_treatment_all %>%
  filter(cc_treatment == "no_cc",
         drying_treatment == "one_wk",
         pre_post_wet != "cw")
bact_no_cc_one_wk %>%
  kruskal.test(data = ., prop_conc_norm ~ pre_post_wet)
bact_no_cc_two_wk <- bact_treatment_all %>%
  filter(cc_treatment == "no_cc",
         drying_treatment == "two_wk",
         pre_post_wet != "cw")
bact_no_cc_two_wk %>%
  kruskal.test(data = .,  prop_conc_norm ~ pre_post_wet)
bact_no_cc_four_wk <- bact_treatment_all %>%
  filter(cc_treatment == "no_cc",
         drying_treatment == "four_wk",
         pre_post_wet != "cw")
bact_no_cc_four_wk %>%
  kruskal.test(data = .,  prop_conc_norm ~ pre_post_wet)

# Make plot and run Kruskal-Wallis test to see effect of rewetting on
# bacterial DNA quantities in samples with cc
bact_rewet_w_cc <- analyze_plot_dna(bact_treatment_all, "w_cc")
bact_rewet_stats_w_cc <- bact_rewet_w_cc[1]
bact_rewet_plot_w_cc <- bact_rewet_w_cc[2]
bact_rewet_med_w_cc <- bact_rewet_w_cc[3]
# Every time point in bacteria w cc
bact_w_cc_one_wk <- bact_treatment_all %>%
  filter(cc_treatment == "w_cc",
         drying_treatment == "one_wk",
         pre_post_wet != "cw")
bact_w_cc_one_wk %>%
  kruskal.test(data = ., prop_conc_norm ~ pre_post_wet)
bact_w_cc_two_wk <- bact_treatment_all %>%
  filter(cc_treatment == "w_cc",
         drying_treatment == "two_wk",
         pre_post_wet != "cw")
bact_w_cc_two_wk %>%
  kruskal.test(data = .,  prop_conc_norm ~ pre_post_wet)
bact_w_cc_four_wk <- bact_treatment_all %>%
  filter(cc_treatment == "w_cc",
         drying_treatment == "four_wk",
         pre_post_wet != "cw")
bact_w_cc_four_wk %>%
  kruskal.test(data = .,  prop_conc_norm ~ pre_post_wet)

# Make plot and run Kruskal-Wallis test to see effect of rewetting on
# fungal DNA quantities in samples with cc
fung_rewet_w_cc <- analyze_plot_dna(fung_treatment_all, "w_cc")
fung_rewet_stats_w_cc <- fung_rewet_w_cc[1]
fung_rewet_plot_w_cc <- fung_rewet_w_cc[2]
fung_rewet_med_w_cc <- fung_rewet_w_cc[3]


