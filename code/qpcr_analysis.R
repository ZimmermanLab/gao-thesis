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
files <- dir(
  "data/raw_data/qPCR/", recursive = TRUE, full.names = TRUE,
  pattern = "\\Quantification Cq Results.csv")

# Read in data from csv files
raw_data <- read_csv(files)

# Clean up compiled data
clean_data <- raw_data %>%
  filter(Fluor == "SYBR") %>%
  select(Sample, Cq)

# Separate out replicate numbers
clean_data_all <- clean_data %>%
  rename("sample_no_full" = Sample, "cq" = Cq) %>%
  mutate("sample_no" = as.numeric(str_sub(sample_no_full, end = 3))) %>%
  mutate("rep_no" = as.numeric(str_sub(sample_no_full, start = -1)))

# Filter out NA values and store them separately
na_only <- clean_data_all %>%
  filter(cq == "NaN") %>%
  filter(sample_no_full != "Blank")
clean_data_samples <- clean_data_all %>%
  filter(cq != "NaN") %>%
  filter(sample_no_full != "Blank") %>%
  select(sample_no, rep_no, cq) %>%
  arrange(sample_no)

# Bring in master list of jar assignments
all_treatments <- readr::read_csv("output/2022/jar_assignments/master_list.csv")

# Calculate means and SDs per sample
qpcr_stats <- clean_data_all %>%
  group_by(sample_no) %>%
  summarize(mean_cq = mean(cq),
            sd_cq = sd(cq))

# Calculate SD difference of each replicate to determine outliers
# stats_clean <- qpcr_data_stats %>%
#   mutate(outlier = (cq >= mean_cq + 0.3) | (cq <= mean_cq - 0.3))

# Map to jar assignments and calculate stats per treatment
qpcr_stats_treatment <- qpcr_stats %>%
  left_join(all_treatments) %>%
  group_by(pre_post_wet, cc_treatment, drying_treatment) %>%
  summarize(mean_all_cq = mean(mean_cq),
            sd_all_cq = sd(sd_cq)) %>%
  mutate(time = paste0(drying_treatment, "_", pre_post_wet))

# Plot
dna_plot_grid <- qpcr_stats_treatment %>%
  filter(pre_post_wet != "no_soil") %>%
  group_by(pre_post_wet, cc_treatment, drying_treatment) %>%
  ggplot(aes(x = time,
             y = mean_all_cq,
             fill = cc_treatment)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(aes(ymax = mean_all_cq + sd_all_cq,
                    ymin = mean_all_cq - sd_all_cq),
                size = 0.25,
                width = 0.2,
                position = position_dodge(0.9))



+
  scale_x_discrete(limits = c("all_dry", "cw", "pre", "post"),
                   labels = c("All Dry", "Constant Water", "Pre Wetting",
                              "Post Wetting")) +
  coord_cartesian(ylim=c(0, 8)) +
  labs(title = "DNA concentrations in Samples With and Without Cover Crop\n",
                     " Residue in Soil Extracts",
       x = "Water Treatments", y = "DNA Concentration",
       fill = "Cover Crop Treatment") +
  scale_fill_discrete(breaks = c("w_cc", "no_cc"),
                      labels = c("Without cover crop", "With cover crop"))
