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

# Compile list of file paths of Cq results from runs
files_fungal <- dir_ls(path = "data/raw_data/qPCR/",
                recurse = 1,
                regex = "fungal -  Quantification Cq Results.csv")

# Read in data from csv files of fungal plates only
raw_data <- read_csv(files_fungal)

# Clean up compiled data and remove Blanks
clean_data <- raw_data %>%
  filter(Fluor == "SYBR") %>%
  select(Sample, Cq) %>%
  filter(!(Sample == "Blank"))

# Separate out replicate numbers
clean_data_all <- clean_data %>%
  rename("sample_no_full" = Sample, "cq" = Cq) %>%
  mutate("sample_no" = as.numeric(str_sub(sample_no_full, end = 3))) %>%
  mutate("rep_no" = as.numeric(str_sub(sample_no_full, start = -1)))

# Filter out NA values and store them separately
na_only <- clean_data_all %>%
  filter(cq == "NaN") %>%
  select(sample_no, rep_no, cq)
# Save out need rerun samples
write_csv(na_only, file = paste0("output/2022/",
                                 Sys.Date(), "_fungal_qpcr_reruns.csv"))
clean_data_samples <- clean_data_all %>%
  filter(!(sample_no %in% na_only$sample_no)) %>%
  select(sample_no, rep_no, cq) %>%
  arrange(sample_no)


# Calculate means and SDs per sample
qpcr_stats <- clean_data_samples %>%
  group_by(sample_no) %>%
  summarize(mean_cq = mean(cq),
            sd_cq = sd(cq))

# Bring in dried soil weights and Qubit concentrations
dried_weights <- read_csv("data/raw_data/qPCR/2022_DNA.csv") %>%
  select(sample_no, extraction_soil_wt_mg,
         qubit_concentration, dry_soil_only) %>%
  filter(sample_no != "bacteria-control")
dried_weights$sample_no <- as.double(dried_weights$sample_no)
dried_weights$qubit_concentration <-as.double(
  dried_weights$qubit_concentration)

# Map to qPCR data
qpcr_stats_all <- qpcr_stats %>%
  left_join(dried_weights) %>%
  filter(is.na(dry_soil_only) == FALSE)

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

# Bring in master list of jar assignments
all_treatments <- readr::read_csv("output/2022/jar_assignments/master_list.csv")

# Map to jar assignments and calculate stats per treatment
# using Qubit normalization
qpcr_stats_treatment <- qpcr_stats %>%
  left_join(all_treatments) %>%
  group_by(pre_post_wet, cc_treatment, drying_treatment) %>%
  summarize(mean_mean_cq = mean(mean_cq),
            sd_mean_cq = sd(mean_cq))

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
