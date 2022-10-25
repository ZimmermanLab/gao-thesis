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

### 1. READ IN DATA AND CLEAN

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

### 2. GET STATS FOR SAMPLES AND NORMALIZE TO DRY SOIL WEIGHT

# Get sample stats using function
source("code/functions/qpcr_functions/qpcr_sample_stats.R")
fungal_stats <- qpcr_sample_stats(clean_fungal_no_na)
bacterial_stats <- qpcr_sample_stats(clean_bacterial_no_na)

# Merge fungal and bacterial dataframes
all_stats <- fungal_stats %>%
  rename("fungal_mean_cq" = mean_cq,
         "fungal_sd_cq" = sd_cq) %>%
  mutate(bacterial_mean_cq = bacterial_stats$mean_cq,
         bacterial_sd_cq = bacterial_stats$sd_cq)

# Bring in dried soil weights and Qubit concentrations
dried_wts_qubit <- read_csv("data/raw_data/qPCR/2022_DNA.csv") %>%
  select(sample_no, extraction_soil_wt_mg,
         qubit_concentration, dry_soil_only) %>%
  filter(sample_no != "bacteria-control") %>%
  filter(!(is.na(dry_soil_only)))
dried_wts_qubit$sample_no <- as.double(dried_wts_qubit$sample_no)
dried_wts_qubit$qubit_concentration <-as.double(
  dried_wts_qubit$qubit_concentration)

# Normalize dried soil weights
library("caret")
process_dry_soil <- preProcess(dried_wts_qubit[,c(4)], method = c("range"))
norm_scale_mapped <- predict(process_dry_soil, dried_wts_qubit[,c(1,4)]) %>%
  rename(dry_soil_norm = dry_soil_only)

# Use initial no cc as baseline for delta delta Cq calculations
all_treatments <- read_csv("output/2022/jar_assignments/master_list.csv")
initial_nocc_mean_fungal <- all_treatments %>%
  left_join(all_stats) %>%
  filter(pre_post_wet == "initial",
         cc_treatment == "no_cc") %>%
  summarize(fungal_mean_cq = mean(fungal_mean_cq))

initial_nocc_mean_bacterial <- all_treatments %>%
  left_join(all_stats) %>%
  filter(pre_post_wet == "initial",
         cc_treatment == "no_cc") %>%
  summarize(bacterial_mean_cq = mean(bacterial_mean_cq))

# Map to samples and normalize fungal and bacterial Cq values
norm_cq_all <- all_stats %>%
  left_join(norm_scale_mapped) %>%
  mutate(norm_cq = dry_soil_norm * mean_cq)

# Map to qPCR data
qpcr_stats_all <- qpcr_stats %>%
  left_join(dried_weights) %>%
  filter(is.na(dry_soil_only) == FALSE)

# Test correlation between dry soil weight and qubit concentrations
cor(qpcr_stats_all$dry_soil_only, qpcr_stats_all$qubit_concentration)

# Test correlation between dry soil weight and Cq values
ggplot(qpcr_stats_all, aes(x = dry_soil_only,
                           y = mean_cq)) + geom_point()
cor(qpcr_stats_all$dry_soil_only, qpcr_stats_all$mean_cq)
# Normalize Cq values based on dried soil weights
qpcr_soil_standardized <- qpcr_stats_all %>%
  mutate(dry_soil_norm = (scale(dry_soil_only, center = FALSE))) %>%
  mutate(cq_normalized = mean_cq * dry_soil_norm)


# Test correlation between total Qubit DNA and Cq values
ggplot(qpcr_stats_all, aes(x = qubit_concentration,
                           y = mean_cq)) + geom_point()
cor(qpcr_stats_all$qubit_concentration, qpcr_stats_all$mean_cq)
# Normalize Cq values based on Qubit concentrations
qpcr_qubit_standardized <- qpcr_stats_all %>%
  mutate("qubit_norm" = (scale(qubit_concentration, center = FALSE))) %>%
  mutate("cq_normalized" = mean_cq / qubit_norm)




# Plot
dna_plot_grid <- qpcr_stats_treatment %>%
  group_by(pre_post_wet, cc_treatment, drying_treatment) %>%
  ggplot(aes(x = pre_post_wet,
             y = mean_mean_cq,
             fill = cc_treatment)) +
  geom_col(position = position_dodge()) +
  facet_grid(. ~ factor(drying_treatment,
                        levels = c("initial", "one_wk", "two_wk",
                                   "four_wk", "all_dry"))) +
  geom_errorbar(aes(ymax = mean_mean_cq + sd_mean_cq,
                    ymin = mean_mean_cq - sd_mean_cq),
                size = 0.25,
                width = 0.2,
                position = position_dodge(0.9)) +
  coord_cartesian(ylim=c(0, 40)) +
  scale_x_discrete(limits = c("all_dry", "initial", "cw", "pre", "post"))
,
                   labels = c("All Dry", "Constant Water", "Pre Wetting",
                              "Post Wetting"))


+
  coord_cartesian(ylim=c(0, 8)) +
  labs(title = "DNA concentrations in Samples With and Without Cover Crop\n",
                     " Residue in Soil Extracts",
       x = "Water Treatments", y = "DNA Concentration",
       fill = "Cover Crop Treatment") +
  scale_fill_discrete(breaks = c("w_cc", "no_cc"),
                      labels = c("Without cover crop", "With cover crop"))


#############



# BACTERIAL


clean_bacterial_all <- left_join(clean_bacterial_data_1, clean_bacterial_data_2) %>%
  rename("sample_no_full" = Sample, "cq" = Cq) %>%
  mutate("sample_no" = as.numeric(str_sub(sample_no_full, end = 3))) %>%
  mutate("rep_no" = as.numeric(str_sub(sample_no_full, start = -1)))

clean_bacterial_stats <- clean_bacterial_all %>%
  select(sample_no, cq) %>%
  group_by(sample_no) %>%
  filter(!(is.na(cq))) %>%
  filter(!(cq > 30)) %>%
  summarize(cq,
    mean_cq = mean(cq),
            sd_cq = sd(cq)) %>%
  mutate(flag = case_when(cq > 5+mean_cq ~ "yes"))

na_only_bacterial <- clean_bacterial_stats %>%
  filter(cq == "NaN" | cq > mean_cq + 2*sd_cq | cq < mean_cq - 2*sd_cq)

# Map to jar assignments and calculate stats per treatment
bacterial_stats_treatment <- clean_bacterial_stats %>%
  filter(is.na(flag)) %>%
  left_join(all_treatments) %>%
  group_by(pre_post_wet, cc_treatment, drying_treatment) %>%
  summarize(mean_mean_cq = mean(mean_cq),
            sd_mean_cq = sd(mean_cq))
