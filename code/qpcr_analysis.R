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

# Compile list of file paths for fungal data
files_fungal <- dir_ls(path = "data/raw_data/qPCR/",
                recurse = 1,
                regex = "fungal -  Quantification Cq Results.csv")

# Compile list of file paths for bacterial data (full csv files from machine 1)
files_bacterial_full <- dir_ls(path = "data/raw_data/qPCR/",
                            recurse = 1,
                            regex =
                              "bacterial -  Quantification Cq Results.csv$")

# Clean fungal data
clean_fungal <- clean_qpcr_data(files_fungal)

# Compile list of file paths for bacterial data (short csv files from machine 2)
files_bacterial_short <- dir_ls(path = "data/raw_data/qPCR/",
                            recurse = 1,
                            regex =
                              "bacterial.csv$")

# Get clean bacterial data using function
clean_bacterial <- clean_qpcr_data(files_bacterial_full) %>%
  rbind(clean_qpcr_data(files_bacterial_short))

# Filter out NA values and store them separately
na_only_fungal <- clean_fungal %>%
  filter(cq == "NaN") %>%
  select(sample_no, rep_no, cq)
na_only_bacterial <- clean_bacterial %>%
  filter(cq == "NaN") %>%
  select(sample_no, rep_no, cq)

# Remove NA samples from the datasets
clean_bacterial_no_na <- clean_bacterial %>%
  filter(!(is.na(cq))) %>%
  select(sample_no, rep_no, cq) %>%
  arrange(sample_no)
clean_fungal_no_na <- clean_fungal %>%
  filter(!(is.na(cq))) %>%
  select(sample_no, rep_no, cq) %>%
  arrange(sample_no)

# Bring in treatment mapping
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

# Get sample stats using function
source("code/functions/qpcr_functions/qpcr_sample_stats.R")

# Use all data (includes all outliers)
fungal_stats_all <- qpcr_sample_stats(fungal_outlier_flags)
bacterial_stats_all <- qpcr_sample_stats(bacterial_outlier_flags)
# Filter out extreme outliers (> 3x IQR)
fungal_stats_no_extreme <- fungal_outlier_flags %>%
  filter(!(outlier_flag == "extreme") |
           is.na(outlier_flag)) %>%
  qpcr_sample_stats()
bacterial_stats_no_extreme <- bacterial_outlier_flags %>%
  filter(!(outlier_flag == "extreme") |
           is.na(outlier_flag)) %>%
  qpcr_sample_stats()
# Filter out extreme and moderate outliers (> 1.5x IQR)
fungal_stats_no_moderate <- fungal_outlier_flags %>%
  filter(is.na(outlier_flag)) %>%
  qpcr_sample_stats()
bacterial_stats_no_moderate <- bacterial_outlier_flags %>%
  filter(is.na(outlier_flag)) %>%
  qpcr_sample_stats()

# Merge fungal and bacterial dataframes
# With outliers included
all_stats_all <- fungal_stats_all %>%
  rename("fungal_mean_cq" = mean_cq,
         "fungal_sd_cq" = sd_cq) %>%
  mutate(bacterial_mean_cq = bacterial_stats_all$mean_cq,
         bacterial_sd_cq = bacterial_stats_all$sd_cq)
# With extreme outliers filtered out
all_stats_no_extreme <- fungal_stats_no_extreme %>%
  rename("fungal_mean_cq" = mean_cq,
         "fungal_sd_cq" = sd_cq) %>%
  mutate(bacterial_mean_cq = bacterial_stats_no_extreme$mean_cq,
         bacterial_sd_cq = bacterial_stats_no_extreme$sd_cq)
# With moderate and extreme outliers filtered out
all_stats_no_moderate <- fungal_stats_no_moderate %>%
  rename("fungal_mean_cq" = mean_cq,
         "fungal_sd_cq" = sd_cq) %>%
  mutate(bacterial_mean_cq = bacterial_stats_no_moderate$mean_cq,
         bacterial_sd_cq = bacterial_stats_no_moderate$sd_cq)

# Find proportional starting concentrations using 2^-Cq
source("code/functions/qpcr_functions/calculate_prop_concentrations.R")
concentrations_all <- calc_prop_conc(all_stats_all)
concentrations_no_extreme <- calc_prop_conc(all_stats_no_extreme)
concentrations_no_moderate <- calc_prop_conc(all_stats_no_moderate)

# Bring in dried soil weights and Qubit concentrations
dried_wts_qubit <- read_csv("data/raw_data/qPCR/2022_DNA.csv") %>%
  select(sample_no, extraction_soil_wt_mg,
         qubit_concentration, dry_soil_only) %>%
  filter(sample_no != "bacteria-control") %>%
  filter(!(is.na(dry_soil_only)))
dried_wts_qubit$sample_no <- as.double(dried_wts_qubit$sample_no)
dried_wts_qubit$qubit_concentration <-as.double(
  dried_wts_qubit$qubit_concentration)

####
# Test correlation between dry soil weight and qubit concentrations
cor(qpcr_stats_all$dry_soil_only, qpcr_stats_all$qubit_concentration)
# Test correlation between total Qubit DNA and Cq values
ggplot(qpcr_stats_all, aes(x = qubit_concentration,
                           y = mean_cq)) + geom_point()
cor(qpcr_stats_all$qubit_concentration, qpcr_stats_all$mean_cq)
# Test correlation between dry soil weight and Cq values
ggplot(qpcr_stats_all, aes(x = dry_soil_only,
                           y = mean_cq)) + geom_point()
cor(qpcr_stats_all$dry_soil_only, qpcr_stats_all$mean_cq)
####

# Normalize the proportional concentrations and calculate
# fungal:bacterial ratios
source("code/functions/qpcr_functions/find_fung_bact_ratio.R")
ratios_all <- calc_ratios(concentrations_all, dried_wts_qubit)
ratios_no_extreme <- calc_ratios(concentrations_no_extreme, dried_wts_qubit)
ratios_no_moderate <- calc_ratios(concentrations_no_moderate, dried_wts_qubit)

# Map samples to all treatment conditions and get summary statistics
source("code/functions/qpcr_functions/calc_treatment_stats.R")
treatment_stats_all <- calc_treatment_stats(ratios_all, all_treatments)
treatment_stats_no_ext <- calc_treatment_stats(ratios_no_extreme, all_treatments)
treatment_stats_no_mod <- calc_treatment_stats(ratios_no_moderate, all_treatments)

# Plot ratios
source("code/functions/qpcr_functions/plot_fung_bact_ratios.R")
all_data_ratio_plot <- plot_dna(treatment_stats_all, "ratio")
all_data_bact_plot <- plot_dna(treatment_stats_all, "bacterial")
all_data_fung_plot <- plot_dna(treatment_stats_all, "fungal")

no_extreme_ratio_plot <- plot_dna(treatment_stats_no_ext, "ratio")
no_extreme_bact_plot <- plot_dna(treatment_stats_no_ext, "bacterial")
no_extreme_fung_plot <- plot_dna(treatment_stats_no_ext, "fungal")

no_moderate_ratio_plot <- plot_dna(treatment_stats_no_mod, "ratio")
no_moderate_bact_plot <- plot_dna(treatment_stats_no_mod, "bacterial")
no_moderate_fung_plot <- plot_dna(treatment_stats_no_mod, "fungal")

ggsave(plot = all_data_ratio_plot,
       filename = "output/2022/qpcr_plots/ratio_all_data_plot.png",
       width = 14, height = 8, units = "in")
ggsave(plot = no_extreme_ratio_plot,
       filename = "output/2022/qpcr_plots/ratio_no_extreme_plot.png",
       width = 14, height = 8, units = "in")
ggsave(plot = no_moderate_ratio_plot,
       filename = "output/2022/qpcr_plots/ratio_no_mod_plot.png",
       width = 14, height = 8, units = "in")

ggsave(plot = all_data_bact_plot,
       filename = "output/2022/qpcr_plots/bact_all_data_plot.png",
       width = 14, height = 8, units = "in")
ggsave(plot = no_extreme_bact_plot,
       filename = "output/2022/qpcr_plots/bact_no_extreme_plot.png",
       width = 14, height = 8, units = "in")
ggsave(plot = no_moderate_bact_plot,
       filename = "output/2022/qpcr_plots/bact_no_mod_plot.png",
       width = 14, height = 8, units = "in")

ggsave(plot = all_data_fung_plot,
       filename = "output/2022/qpcr_plots/fung_all_data_plot.png",
       width = 14, height = 8, units = "in")
ggsave(plot = no_extreme_fung_plot,
       filename = "output/2022/qpcr_plots/fung_no_extreme_plot.png",
       width = 14, height = 8, units = "in")
ggsave(plot = no_moderate_fung_plot,
       filename = "output/2022/qpcr_plots/fung_no_mod_plot.png",
       width = 14, height = 8, units = "in")

  coord_cartesian(ylim=c(0, 8)) +
  labs(title = "DNA concentrations in Samples With and Without Cover Crop\n",
                     " Residue in Soil Extracts",
       x = "Water Treatments", y = "DNA Concentration",
       fill = "Cover Crop Treatment") +
  scale_fill_discrete(breaks = c("w_cc", "no_cc"),
                      labels = c("Without cover crop", "With cover crop"))


#############






# Use mean / Cq for normalization of dry soil weight
# Plot, normalize, and plot -> should be a flat line
# 2^-Cq = proportional starting concentration
# Proportional starting concentration * 2^-Cq = 1
# Normalize with starting proportion and then normalize to dry soil weight

# Use median to calculate outliers
# Throw out outliers beyond 1.5*inner quartile range
# IQR()
# > 1.5 is mild outlier, > 3 is extreme outlier
# Run analyses with everything, talk about it
# Run analyses with extreme outliers removed, talk about it
