# This script organizes and analyzes results from the SmartChem measurements
# of nitrite, nitrate, and ammonia.

# Sarah Gao
# December 2, 2021
# hellosarahgao@gmail.com

# Source and run function to clean N data
source("code/functions/n_functions/n_clean_n_data.R")
n_data_clean <- clean_n_data(
  "data/raw_data/SmartChem_N_extractions/20210212_Gao_1_export.csv")

# Source and run function to analyze stats for samples
source("code/functions/n_functions/n_calculate_sample_stats.R")
n_sample_stats <- n_calculate_sample_stats(n_data_clean)

# Source and run function that compiles all the bad jar assignment
# csvs from Spring 2021
source("code/functions/compile_sample_assignments.R")
all_treatments <- compile_sample_assignments()

# Source  and run function that organizes and aggregates n results to
# master list of treatments
source("code/functions/n_functions/n_aggregate_results.R")
all_treatments_n <- add_n_results(all_treatments, n_sample_stats)

# Store means of specific drying treatment x cc treatment intersections
mean_wk4_no_cc_cw_nh3 <- all_treatments_n %>%
  filter(drying_treatment == "four_wk_cw" && cc_treatment == "no_cc") %>%
  pull(mean_nh3) %>%
  mean()

mean_wk4_no_cc_cw_no2 <- all_treatments_n %>%
  filter(drying_treatment == "four_wk_cw" && cc_treatment == "no_cc") %>%
  pull(mean_no2) %>%
  mean()

mean_wk4_no_cc_cw_no2_no3 <- all_treatments_n %>%
  filter(drying_treatment == "four_wk_cw" && cc_treatment == "no_cc") %>%
  pull(mean_no2_no3) %>%
  mean()

mean_wk4_w_cc_cw_nh3 <- all_treatments_n %>%
  filter(drying_treatment == "four_wk_cw" && cc_treatment == "w_cc") %>%
  pull(mean_nh3) %>%
  mean()

mean_wk4_w_cc_cw_no2 <- all_treatments_n %>%
  filter(drying_treatment == "four_wk_cw" && cc_treatment == "no_cc") %>%
  pull(mean_no2) %>%
  mean()

mean_wk4_w_cc_cw_no2_no3 <- all_treatments_n %>%
  filter(drying_treatment == "four_wk_cw" && cc_treatment == "no_cc") %>%
  pull(mean_no2_no3) %>%
  mean()

mean_wk4_no_cc_post_nh3 <- all_treatments_n %>%
  filter(drying_treatment == "four_wk" &&
           pre_post_wet == "post" && cc_treatment == "no_cc") %>%
  pull(mean_nh3) %>%
  mean()

mean_wk4_no_cc_post_no2 <- all_treatments_n %>%
  filter(drying_treatment == "four_wk" &&
           pre_post_wet == "post" && cc_treatment == "no_cc") %>%
  pull(mean_no2) %>%
  mean()

mean_wk4_no_cc_post_no2_no3 <- all_treatments_n %>%
  filter(drying_treatment == "four_wk" &&
           pre_post_wet == "post" && cc_treatment == "no_cc") %>%
  pull(mean_no2_no3) %>%
  mean()

mean_wk4_w_cc_post_nh3 <- all_treatments_n %>%
  filter(drying_treatment == "four_wk" &&
           pre_post_wet == "post" && cc_treatment == "w_cc") %>%
  pull(mean_nh3) %>%
  mean()

mean_wk4_w_cc_post_no2 <- all_treatments_n %>%
  filter(drying_treatment == "four_wk" &&
           pre_post_wet == "post" && cc_treatment == "w_cc") %>%
  pull(mean_no2) %>%
  mean()

mean_wk4_w_cc_post_no2_no3 <- all_treatments_n %>%
  filter(drying_treatment == "four_wk" &&
           pre_post_wet == "post" && cc_treatment == "w_cc") %>%
  pull(mean_no2_no3) %>%
  mean()

mean_wk4_no_cc_pre_nh3 <- all_treatments_n %>%
  filter(drying_treatment == "four_wk" &&
           pre_post_wet == "pre" && cc_treatment == "no_cc") %>%
  pull(mean_nh3) %>%
  mean()

mean_wk4_no_cc_pre_no2 <- all_treatments_n %>%
  filter(drying_treatment == "four_wk" &&
           pre_post_wet == "pre" && cc_treatment == "no_cc") %>%
  pull(mean_no2) %>%
  mean()

mean_wk4_no_cc_pre_no2_no3 <- all_treatments_n %>%
  filter(drying_treatment == "four_wk" &&
           pre_post_wet == "pre" && cc_treatment == "no_cc") %>%
  pull(mean_no2_no3) %>%
  mean()

mean_wk4_w_cc_pre_nh3 <- all_treatments_n %>%
  filter(drying_treatment == "four_wk" &&
           pre_post_wet == "pre" && cc_treatment == "w_cc") %>%
  pull(mean_nh3) %>%
  mean()

mean_wk4_w_cc_pre_no2 <- all_treatments_n %>%
  filter(drying_treatment == "four_wk" &&
           pre_post_wet == "pre" && cc_treatment == "w_cc") %>%
  pull(mean_no2) %>%
  mean()

mean_wk4_w_cc_pre_no2_no3 <- all_treatments_n %>%
  filter(drying_treatment == "four_wk" &&
           pre_post_wet == "pre" && cc_treatment == "w_cc") %>%
  pull(mean_no2_no3) %>%
  mean()

# Create a plot comparing NO2-NO3 levels with cover crop at 4 weeks
means_w_cc_no2_no3 <- data.frame(
  mean_wk4_w_cc_cw_no2_no3, mean_wk4_w_cc_pre_no2_no3,
  mean_wk4_w_cc_post_no2_no3) %>%
  pivot_longer(cols = 1:3, names_to = "names") %>%
  mutate(names = str_replace(names, "mean_wk4_w_cc_", "")) %>%
  mutate(names = str_replace(names, "_no2_no3", ""))

means_w_cc_no2_no3_plot <- ggplot(means_w_cc_no2_no3,
                                  aes(x = names, y = value)) +
  geom_col() +
  scale_y_continuous() +
  scale_x_discrete(limits = c("cw", "pre", "post"),
                   labels = c(
                     "Constant Water", "Pre Wetting", "Post Wetting")) +
  labs(title = "NO2-NO3 Values in Samples With Cover Crop Residue",
       x = "Water Treatments", y = "NO2-NO3 (ppm)")

# Create a plot comparing NO2-NO3 levels without cover crop at 4 weeks
means_no_cc_no2_no3 <- data.frame(
  mean_wk4_no_cc_cw_no2_no3, mean_wk4_no_cc_pre_no2_no3,
  mean_wk4_no_cc_post_no2_no3) %>%
  pivot_longer(cols = 1:3, names_to = "names") %>%
  mutate(names = str_replace(names, "mean_wk4_no_cc_", "")) %>%
  mutate(names = str_replace(names, "_no2_no3", ""))

means_no_cc_no2_no3_plot <- ggplot(means_no_cc_no2_no3,
                                   aes(x = names, y = value)) +
  geom_col() +
  scale_y_continuous() +
  scale_x_discrete(limits = c("cw", "pre", "post"),
                   labels = c(
                     "Constant Water", "Pre Wetting", "Post Wetting")) +
  labs(title = "NO2-NO3 Values in Samples Without Cover Crop Residue",
       x = "Water Treatments", y = "NO2-NO3 (ppm)")

# Create a plot comparing NH3 levels with cover crop at 4 weeks
means_w_cc_nh3 <- data.frame(
  mean_wk4_w_cc_cw_nh3, mean_wk4_w_cc_pre_nh3,
  mean_wk4_w_cc_post_nh3) %>%
  pivot_longer(cols = 1:3, names_to = "names") %>%
  mutate(names = str_replace(names, "mean_wk4_w_cc_", "")) %>%
  mutate(names = str_replace(names, "_nh3", ""))

means_w_cc_nh3_plot <- ggplot(means_w_cc_nh3, aes(x = names, y = value)) +
  geom_col() +
  scale_y_continuous() +
  scale_x_discrete(limits = c("cw", "pre", "post"),
                   labels = c(
                     "Constant Water", "Pre Wetting", "Post Wetting")) +
  labs(title = "NH3 Values in Samples With Cover Crop Residue",
       x = "Water Treatments", y = "NH3 (ppm)")

# Create a plot comparing NH3 levels without cover crop at 4 weeks
means_no_cc_nh3 <- data.frame(
  mean_wk4_no_cc_cw_nh3, mean_wk4_no_cc_pre_nh3,
  mean_wk4_no_cc_post_nh3) %>%
  pivot_longer(cols = 1:3, names_to = "names") %>%
  mutate(names = str_replace(names, "mean_wk4_no_cc_", "")) %>%
  mutate(names = str_replace(names, "_nh3", ""))

means_no_cc_nh3_plot <- ggplot(means_no_cc_nh3, aes(x = names, y = value)) +
  geom_col() +
  scale_y_continuous() +
  scale_x_discrete(limits = c("cw", "pre", "post"),
                   labels = c(
                     "Constant Water", "Pre Wetting", "Post Wetting")) +
  labs(title = "NH3 Values in Samples Without Cover Crop Residue",
       x = "Water Treatments", y = "NH3 (ppm)")

# Create a plot comparing NO2 levels with cover crop at 4 weeks
means_w_cc_no2 <- data.frame(
  mean_wk4_w_cc_cw_no2, mean_wk4_w_cc_pre_no2,
  mean_wk4_w_cc_post_no2) %>%
  pivot_longer(cols = 1:3, names_to = "names") %>%
  mutate(names = str_replace(names, "mean_wk4_w_cc_", "")) %>%
  mutate(names = str_replace(names, "_no2", ""))

means_w_cc_no2_plot <- ggplot(means_w_cc_no2, aes(x = names, y = value)) +
  geom_col() +
  scale_y_continuous() +
  scale_x_discrete(limits = c("cw", "pre", "post"),
                   labels = c(
                     "Constant Water", "Pre Wetting", "Post Wetting")) +
  labs(title = "NO2 Values in Samples With Cover Crop Residue",
       x = "Water Treatments", y = "NO2 (ppm)")

# Create a plot comparing NO2 levels without cover crop at 4 weeks
means_no_cc_no2 <- data.frame(
  mean_wk4_no_cc_cw_no2, mean_wk4_no_cc_pre_no2,
  mean_wk4_no_cc_post_no2) %>%
  pivot_longer(cols = 1:3, names_to = "names") %>%
  mutate(names = str_replace(names, "mean_wk4_no_cc_", "")) %>%
  mutate(names = str_replace(names, "_no2", ""))

means_no_cc_no2_plot <- ggplot(means_no_cc_no2, aes(x = names, y = value)) +
  geom_col() +
  scale_y_continuous() +
  scale_x_discrete(limits = c("cw", "pre", "post"),
                   labels = c(
                     "Constant Water", "Pre Wetting", "Post Wetting")) +
  labs(title = "NO2 Values in Samples Without Cover Crop Residue",
       x = "Water Treatments", y = "NO2 (ppm)")

# Create a plot that shows NO2-NO3 values across drying treatments grouped by
# cover crop treatment
