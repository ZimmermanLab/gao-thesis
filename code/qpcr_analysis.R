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
bact_norm_all <- norm_conc(bact_concentrations, dried_wts_qubit)
fung_norm_all <- norm_conc(fung_concentrations, dried_wts_qubit)

# Map samples to all treatment conditions
bact_treatment_all <- bact_norm_all %>%
  left_join(all_treatments)
fung_treatment_all <- fung_norm_all %>%
  left_join(all_treatments)


# Make plot and run Kruskal-Wallis test to see effect of rewetting on
# DNA quantities
source("code/functions/qpcr_functions/analyze_plot_dna.R")
# cc agnostic
bact_rewet_all <- analyze_plot_dna(bact_treatment_all, "all")
# Run test at every time point to compare differences before and after
source("code/functions/qpcr_functions/compare_rewet_each_wk.R")
bact_wk_all <- compare_rewet_time(bact_treatment_all, "all")

bact_rewet_all_nomod <- analyze_plot_dna(bact_treatment_no_mod, "all")
bact_wk_all_nomod <- compare_rewet_time(bact_treatment_no_mod, "all")

fung_rewet_all <- analyze_plot_dna(fung_treatment_all, "all")
fung_wk_all <- compare_rewet_time(fung_treatment_all, "all")

fung_rewet_all_nomod <- analyze_plot_dna(fung_treatment_no_mod, "all")
fung_wk_all_nomod <- compare_rewet_time(fung_treatment_no_mod, "all")




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
bact_rewet_stats_w_cc <- bact_rewet_w_cc[1]
bact_rewet_plot_w_cc <- bact_rewet_w_cc[2]
bact_rewet_med_w_cc <- bact_rewet_w_cc[3]

# Make plot and run Kruskal-Wallis test to see effect of rewetting on
# fungal DNA quantities in samples with cc
fung_rewet_w_cc <- analyze_plot_dna(fung_treatment_all, "w_cc")
fung_rewet_stats_w_cc <- fung_rewet_w_cc[1]
fung_rewet_plot_w_cc <- fung_rewet_w_cc[2]
fung_rewet_med_w_cc <- fung_rewet_w_cc[3]

bact_wk_w_cc_all <- compare_rewet_time(bact_treatment_all, "w_cc")
fung_wk_w_cc_all <- compare_rewet_time(fung_treatment_all, "w_cc")

#### Checking outliers ####

# Without extreme outliers (> 3x IQR)
bact_treatment_no_ext <- bact_treatment_all %>%
  filter(!(outlier_flag == "extreme") |
           is.na(outlier_flag)) %>%
  select(-c(outlier_flag))

fung_treatment_no_ext <- fung_treatment_all %>%
  filter(!(outlier_flag == "extreme") |
           is.na(outlier_flag)) %>%
  select(-c(outlier_flag))
# Plot + analyse with no extreme outliers
bact_rewet_no_cc_noext <- analyze_plot_dna(bact_treatment_no_ext, "no_cc")
bact_wk_no_cc_noext <- compare_rewet_time(bact_treatment_no_ext, "no_cc")

fung_rewet_no_cc_noext <- analyze_plot_dna(fung_treatment_no_ext, "no_cc")
fung_wk_no_cc_noext <- compare_rewet_time(fung_treatment_no_ext, "no_cc")

bact_rewet_w_cc_noext <- analyze_plot_dna(bact_treatment_no_ext, "w_cc")
bact_wk_w_cc_noext <- compare_rewet_time(bact_treatment_no_ext, "w_cc")

fung_rewet_w_cc_noext <- analyze_plot_dna(fung_treatment_no_ext, "w_cc")
fung_wk_w_cc_noext <- compare_rewet_time(fung_treatment_no_ext, "w_cc")

# Without extreme and moderate outliers (> 1.5x IQR)
fung_treatment_no_mod <- fung_treatment_all %>%
  filter(is.na(outlier_flag)) %>%
  select(-c(outlier_flag))
bact_treatment_no_mod <- bact_treatment_all %>%
  filter(is.na(outlier_flag)) %>%
  select(-c(outlier_flag))
# Plot + analyse with no extreme or moderate outliers
bact_rewet_no_cc_nomod <- analyze_plot_dna(bact_treatment_no_mod, "no_cc")
bact_wk_no_cc_nomod <- compare_rewet_time(bact_treatment_no_mod, "no_cc")

fung_rewet_no_cc_nomod <- analyze_plot_dna(fung_treatment_no_mod, "no_cc")
fung_wk_no_cc_nomod <- compare_rewet_time(fung_treatment_no_mod, "no_cc")

bact_rewet_w_cc_nomod <- analyze_plot_dna(bact_treatment_no_mod, "w_cc")
bact_wk_w_cc_noext <- compare_rewet_time(bact_treatment_no_mod, "w_cc")

fung_rewet_w_cc_nomod <- analyze_plot_dna(fung_treatment_no_mod, "w_cc")
fung_wk_w_cc_nomod <- compare_rewet_time(fung_treatment_no_mod, "w_cc")
