# This script reads in raw data across all the measured variables
# (i.e. EA CN, SmartChem N, Li-COR CO2, and qPCR DNA) and compiles
# it all into one dataframe for further downstream analyses.

# Note that `code/functions/compile_datasets.R` reads in already
# cleaned data for faster compilation

# Sarah Gao
# hellosarahgao@gmail.com
# February 20, 2023

library("dplyr")
library("readr")
library("tidyr")
library("fs")

##### EA DATA #####
# Source function to clean raw EA data
source("code/functions/ea_functions/clean_ea_data.R")

# Compile list of file paths of percentage EA data
files_percent <- dir_ls(path = "data/raw_data/EA_CN/2022/",
                        recurse = 1,
                        regex =
                          "\\w+_Run\\d_(repeat_)*(\\d{2}_)*percent.(xls|XLS)")
# Compile C:N ratio data
files_ratio <- dir_ls(path = "data/raw_data/EA_CN/2022/",
                      recurse = 1,
                      regex = "\\w+_Run\\d_(repeat_)*(\\d{2}_)*ratio.(xls|XLS)")

# Clean both types of data
# Note that the function filters out any weird sample runs that had 0 values
cn_percent_clean <- clean_ea_data(files_percent)
cn_ratio_clean <- clean_ea_data(files_ratio)

# Find medians per sample across replicates
# This also filters out SRM data
cn_per_med <- cn_percent_clean %>%
  filter(!str_detect(sample_no, "SRM")) %>%
  group_by(sample_no) %>%
  summarize(med_n_per = median(n_per),
            med_c_per = median(c_per))
cn_ratio_med <- cn_ratio_clean %>%
  filter(!str_detect(sample_no, "SRM")) %>%
  group_by(sample_no) %>%
  summarize(med_cn = median(c_n_ratio))

# Combine percentage and ratio data for collective CN dataframe
cn_all <- cn_per_med %>%
  left_join(cn_ratio_med) %>%
  left_join(cn_ratio_med) %>%
  mutate(sample_no = as.numeric(sample_no))

##### qPCR DNA data #####
# Source data cleaning function
source("code/functions/qpcr_functions/clean_qpcr_data.R")

# Get clean fungal data
clean_fungal <- clean_qpcr_data("data/raw_data/qPCR/", "fungal")
# Get clean bacterial data
clean_bacterial <- clean_qpcr_data("data/raw_data/qPCR/", "bacterial")

# Find proportional starting concentrations using 2^-Cq
source("code/functions/qpcr_functions/calculate_prop_concentrations.R")
bact_concentrations <- calc_prop_conc(clean_bacterial)
fung_concentrations <- calc_prop_conc(clean_fungal)

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
         cq_bact = cq)
fung_norm_all <- norm_conc(fung_concentrations, dried_wts_qubit) %>%
  rename(conc_norm_fung = prop_conc_norm,
         cq_fung = cq)

# Find median per sample for bacterial and fungal separately
samp_med_bact <- bact_norm_all %>%
  group_by(sample_no) %>%
  summarise(med_prop_conc_bact = median(conc_norm_bact))
samp_med_fung <- fung_norm_all %>%
  group_by(sample_no) %>%
  summarize(med_prop_conc_fung = median(conc_norm_fung))

# Join bacterial and fugnal DNA into one dataset
dna_all <- samp_med_bact %>%
  left_join(samp_med_fung)

# Add a column for fungal:bacterial DNA ratios
# Note that I did this at the sample level vs the tech rep level since fungal
# and bacterial tech reps cannot be mapped to each other
dna_all <- dna_all %>%
  mutate(fung_bact_ratio = med_prop_conc_fung / med_prop_conc_bact) %>%
  # Removes water only samples that had bacteria but no fungi
  filter(!is.na(fung_bact_ratio))

##### #####
