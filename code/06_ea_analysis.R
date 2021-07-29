# Sarah Gao
# July 28, 2021
# This script takes in EA data and evaluates the accuracy
# and precision of the standards.

library("readr")
library("tidyr")
library("dplyr")
library("ggplot2")
library("scales")

# First, we will examine the SRM data to see if there was any drift / trend
# over the run and to determine if we need any correcting factors.

# Read in and clean up EA data
ea_results_raw <- readr::read_csv("data/raw_data/EA_CN/20210727/210727_Run.csv")
ea_results_clean <- ea_results_raw %>%
  select("X2", "X4", "X6", "X7", "X12", "X13")

ea_results_clean <- ea_results_clean[rowSums(
  is.na(ea_results_clean)) != ncol(ea_results_clean),] %>%
  rename("sample" = "X2", "date" = "X4", "type" = "X6", "weight" = "X7",
         "n_per" = "X12", "c_per" = "X13") %>%
  mutate(pos = row_number())

ea_results_clean <- ea_results_clean[colnames(ea_results_clean)[c(7,1:6)]]

# Calculate means and RSDs for both nitrogen and carbon
# for all SRM samples
all_srms <- ea_results_clean %>%
  filter(sample == "SRM")
mean_srm_n <- mean(all_srms$n_per)
rsd_srm_n <- sd(all_srms$n_per)*100 / mean_srm_n

mean_srm_c <- mean(all_srms$c_per)
rsd_srm_c <- sd(all_srms$c_per)*100 / mean_srm_c

srm_n <- ggplot(all_srms,
                aes(x = pos,
                    y = n_per)) +
  geom_line()

srm_c <- ggplot(all_srms,
                aes(x = pos,
                    y = c_per)) +
  geom_line()
