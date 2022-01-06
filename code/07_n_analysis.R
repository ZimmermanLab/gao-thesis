# This script organizes and analyzes results from the SmartChem measurements
# of nitrite, nitrate, and ammonia.

# Sarah Gao
# December 2, 2021
# hellosarahgao@gmail.com

# Load in libraries

library("stringr")

# Source and run function to clean N data
source("code/functions/n_functions/n_clean_n_data.R")
n_data_clean <- clean_n_data(
  "data/raw_data/SmartChem_N_extractions/20210212_Gao_1_export.csv")

# Source and run function to analyze stats for samples
source("code/functions/n_functions/n_calculate_sample_stats.R")
n_sample_stats <- n_calculate_sample_stats(n_data_clean)

# Read in master list of samples by cover crop treatment
# NOTE FOR FUTURE SARAH: Do NOT make your master lists this god awful.
# Please use tidy table formatting. Thx in advance.

# ALSO Please eventually write another script that reads in these values
# from the initial sampling script kthx
cc_list <- readr::read_csv(
  "output/jar_assignments/master_list.csv", col_names =  FALSE) %>%
  select(X2) %>%
  rename(sample_no = X2) %>%
  mutate(cc_treatment = NA)

# Split into separate treatment groups
jar_w_cc_list <- cc_list[2:56, ] %>%
  mutate(cc_treatment = "w_cc")
jar_no_cc_list <- cc_list[59:113, ] %>%
  mutate(cc_treatment = "no_cc")
no_soil_controls <- cc_list[116:120, ] %>%
  mutate(cc_treatment = "no_soil_control")

cc_list <- rbind(jar_w_cc_list, jar_no_cc_list, no_soil_controls)

cc_list$sample_no <- formatC(as.integer(
  cc_list$sample_no), width = 3, format = "d", flag = "0")

cc_list <- cc_list %>% arrange(sample_no)

###### THIS IS A HOT MESS PLEASE IGNORE

# Read in sampling schedule assignments

sampling_schedule <- data.frame()
sampling_schedule <- readr::read_csv(
  "output/schedules/sampling_schedule_master/all_1wk_postwet.csv") %>%
  select(jar_no) %>%
  rename(sample_no = jar_no) %>%
  mutate(drying_treatment = "one_wk", pre_post_wet = "post") %>%
  rbind(
    readr::read_csv(
      "output/schedules/sampling_schedule_master/all_1wk_prewet.csv") %>%
      select(jar_no) %>%
      rename(sample_no = jar_no) %>%
      mutate(drying_treatment = "one_wk", pre_post_wet = "pre") %>%
      rbind(sampling_schedule)) %>%
  rbind(
    readr::read_csv(
      "output/schedules/sampling_schedule_master/all_2wk_postwet.csv") %>%
      select(jar_no) %>%
      rename(sample_no = jar_no) %>%
      mutate(drying_treatment = "two_wk", pre_post_wet = "post") %>%
      rbind(sampling_schedule)) %>%
  rbind(
    readr::read_csv(
      "output/schedules/sampling_schedule_master/all_2wk_prewet.csv") %>%
      select(jar_no) %>%
      rename(sample_no = jar_no) %>%
      mutate(drying_treatment = "two_wk", pre_post_wet = "pre") %>%
      rbind(sampling_schedule)) %>%
  rbind(
    readr::read_csv(
      "output/schedules/sampling_schedule_master/all_4wk_prewet.csv") %>%
      select(jar_no) %>%
      rename(sample_no = jar_no) %>%
      mutate(drying_treatment = "four_wk", pre_post_wet = "pre") %>%
      rbind(sampling_schedule)) %>%
  rbind(
    readr::read_csv(
      "output/schedules/sampling_schedule_master/all_4wk_total.csv") %>%
      select(jar_no) %>%
      rename(sample_no = jar_no) %>%
      mutate(drying_treatment = "four_wk", pre_post_wet = NA) %>%
      rbind(sampling_schedule)) %>%
  rbind(
    readr::read_csv(
      "output/schedules/all_drought_wt.csv") %>%
      select(jar_no) %>%
      rename(sample_no = jar_no) %>%
      mutate(drying_treatment = "four_wk", pre_post_wet = "drought") %>%
      rbind(sampling_schedule)) %>%
  rbind(
    readr::read_csv(
      "output/schedules/sampling_schedule_master/initial_sample_cw_all.csv") %>%
      select(jar_no) %>%
      rename(sample_no = jar_no) %>%
      mutate(drying_treatment = "initial", pre_post_wet = "pre") %>%
      rbind(sampling_schedule)) %>%
  rbind(
    readr::read_csv("output/jar_assignments/no_soil_controls.csv") %>%
      select(no_soil_controls) %>%
      rename(sample_no = no_soil_controls) %>%
      mutate(drying_treatment = "no_soil", pre_post_wet = "no_soil") %>%
      rbind(sampling_schedule)) %>%
  arrange(sample_no)

sampling_schedule[is.na(sampling_schedule)] <- "post"

# Add leading zeros to sample_no
sampling_schedule$sample_no <- formatC(
  as.integer(sampling_schedule$sample_no), width = 3, format = "d", flag = "0")

# Remove dupes from weird csv files of week 4 ugh
sampling_schedule <- sampling_schedule %>%
  distinct(sample_no, .keep_all = TRUE)

# Account for constantly watered samples ugh
cw_samples <- readr::read_csv("output/jar_assignments/w_cc_cw.csv") %>%
  select(jar_w_cc_cw) %>%
  rename(sample_no = jar_w_cc_cw) %>%
  mutate(drying_treatment = "cw", pre_post_wet = NA, cc_treatment = "w_cc") %>%
  rbind(
    readr::read_csv("output/jar_assignments/no_cc_cw.csv") %>%
    select(jar_no_cc_cw) %>%
    rename(sample_no = jar_no_cc_cw) %>%
    mutate(drying_treatment = "cw",
           pre_post_wet = NA, cc_treatment = "no_cc")) %>%
  arrange(sample_no)

# Add leading zeros to sample_no
cw_samples$sample_no <- formatC(
  as.integer(cw_samples$sample_no), width = 3, format = "d", flag = "0")

# Combine drying treatment and pre/post wet info with cc treatment dataframe
all_treatments <- merge(sampling_schedule, cc_list, by = "sample_no")

# Replace drying_treatment values for all constant watered samples
# Again, due to previous idiocy
for (row in 1:length(cw_samples$sample_no)) {
  match_row_num <- which(grepl(
    cw_samples$sample_no[row], all_treatments$sample_no))
  all_treatments$drying_treatment[match_row_num] <- paste0(
    all_treatments$drying_treatment[match_row_num], "_",
    cw_samples$drying_treatment[row])
}

# Aggregate statistical data by treatment group
n_sample_stats <- n_sample_stats %>%
  mutate(drying_treatment = NA, pre_post_wet = NA)

wk4_w_cc_pre <- data.frame()
wk4_no_cc_pre <- data.frame()
wk4_w_cc_post <- data.frame()
wk4_no_cc_post <- data.frame()
wk4_w_cc_cw <- data.frame()
wk4_no_cc_cw <- data.frame()

for (row in 1:length(n_sample_stats$sample_no)) {
  sample_cc <- all_treatments$cc_treatment[which(
    grepl((n_sample_stats$sample_no[row]), all_treatments$sample_no))]
  sample_pre_post_wet <- all_treatments$pre_post_wet[which(
    grepl((n_sample_stats$sample_no[row]), all_treatments$sample_no))]
  sample_drying_treatment <- all_treatments$drying_treatment[which(
    grepl((n_sample_stats$sample_no[row]), all_treatments$sample_no))]

  if (sample_cc == "no_cc" && sample_pre_post_wet == "pre" &&
      sample_drying_treatment == "four_wk") {
    wk4_no_cc_pre <- rbind(wk4_no_cc_pre, n_sample_stats[row, ])
  }

  if (sample_cc == "w_cc" && sample_pre_post_wet == "pre" &&
      sample_drying_treatment == "four_wk") {
    wk4_w_cc_pre <- rbind(wk4_w_cc_pre, n_sample_stats[row, ])
  }

  if (sample_cc == "no_cc" && sample_pre_post_wet == "post"
      && sample_drying_treatment == "four_wk") {
    wk4_no_cc_post <- rbind(wk4_no_cc_post, n_sample_stats[row, ])
  }

  if (sample_cc == "w_cc" && sample_pre_post_wet == "post"
      && sample_drying_treatment == "four_wk") {
    wk4_w_cc_post <- rbind(wk4_w_cc_post, n_sample_stats[row, ])
  }

  if (sample_cc == "no_cc" && sample_pre_post_wet == "pre"
      && sample_drying_treatment == "four_wk_cw") {
    wk4_no_cc_cw <- rbind(wk4_no_cc_cw, n_sample_stats[row, ])
  }

  if (sample_cc == "w_cc" && sample_pre_post_wet == "pre"
      && sample_drying_treatment == "four_wk_cw") {
    wk4_w_cc_cw <- rbind(wk4_w_cc_cw, n_sample_stats[row, ])
  }
}

# Store means of specific drying treatment x cc treatment intersections
mean_wk4_no_cc_cw_nh3 <- mean(wk4_no_cc_cw$mean_nh3)
mean_wk4_no_cc_cw_no2 <- mean(wk4_no_cc_cw$mean_no2)
mean_wk4_no_cc_cw_no2_no3 <- mean(wk4_no_cc_cw$mean_no2_no3)

mean_wk4_w_cc_cw_nh3 <- mean(wk4_w_cc_cw$mean_nh3)
mean_wk4_w_cc_cw_no2 <- mean(wk4_w_cc_cw$mean_no2)
mean_wk4_w_cc_cw_no2_no3 <- mean(wk4_w_cc_cw$mean_no2_no3)

mean_wk4_no_cc_post_nh3 <- mean(wk4_no_cc_post$mean_nh3)
mean_wk4_no_cc_post_no2 <- mean(wk4_no_cc_post$mean_no2)
mean_wk4_no_cc_post_no2_no3 <- mean(wk4_no_cc_post$mean_no2_no3)

mean_wk4_w_cc_post_nh3 <- mean(wk4_w_cc_post$mean_nh3)
mean_wk4_w_cc_post_no2 <- mean(wk4_w_cc_post$mean_no2)
mean_wk4_w_cc_post_no2_no3 <- mean(wk4_w_cc_post$mean_no2_no3)

mean_wk4_no_cc_pre_nh3 <- mean(wk4_no_cc_pre$mean_nh3)
mean_wk4_no_cc_pre_no2 <- mean(wk4_no_cc_pre$mean_no2)
mean_wk4_no_cc_pre_no2_no3 <- mean(wk4_no_cc_pre$mean_no2_no3)

mean_wk4_w_cc_pre_nh3 <- mean(wk4_w_cc_pre$mean_nh3)
mean_wk4_w_cc_pre_no2 <- mean(wk4_w_cc_pre$mean_no2)
mean_wk4_w_cc_pre_no2_no3 <- mean(wk4_w_cc_pre$mean_no2_no3)

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
