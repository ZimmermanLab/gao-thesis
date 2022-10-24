# This script examines whether the EA run from 8/31/22 is worth re-running or not
# due to discrepancies between samples run then and re-run samples from 10/18/22

# Sarah Gao
#  October 19, 2022
# hellosarahgao@gmail.com

library("readr")
library("tidyr")
library("dplyr")
library("ggplot2")
library("scales")
library("readxl")
library("fs")
library("stringr")


files_percent <- dir_ls(path = "data/raw_data/EA_CN/2022/",
                        recurse = 1,
                        regex = "\\w+_Run\\d_(repeat_)*(\\d{2}_)*percent.(xls|XLS)")

run4 <- str_subset(files_percent, "Run4")
files_filtered <- setdiff(files_percent, run4)

source("code/functions/ea_functions/clean_ea_data.R")

clean_run4 <- clean_ea_data(run4)
clean_run4$sample_no <- as.numeric(str_sub(
  clean_run4$sample_no, start = -3))

clean_filtered <- clean_ea_data(files_filtered)
clean_filtered$sample_no <- as.numeric(str_sub(
  clean_filtered$sample_no, start = -3))

all_treatments <- readr::read_csv("output/2022/jar_assignments/master_list.csv")

mapped_run4 <- clean_run4 %>%
  group_by(sample_no) %>%
  summarize(mean_n = mean(n_mg),
            mean_c = mean(c_mg),
            sd_n = sd(n_mg),
            sd_c = sd(c_mg)) %>%
  left_join(all_treatments) %>%
  group_by(pre_post_wet, cc_treatment, drying_treatment) %>%
  summarize(mean_mean_n = mean(mean_n),
            mean_mean_c = mean(mean_c),
            sd_mean_n = sd(mean_n),
            sd_mean_c = sd(mean_c))

mapped_filtered <- clean_filtered %>%
  group_by(sample_no) %>%
  summarize(mean_n = mean(n_mg),
            mean_c = mean(c_mg),
            sd_n = sd(n_mg),
            sd_c = sd(c_mg)) %>%
  left_join(all_treatments) %>%
  group_by(pre_post_wet, cc_treatment, drying_treatment) %>%
  summarize(mean_mean_n = mean(mean_n),
            mean_mean_c = mean(mean_c),
            sd_mean_n = sd(mean_n),
            sd_mean_c = sd(mean_c))

filtered_v_run4 <- mapped_filtered %>%
  full_join(mapped_run4, by=c("pre_post_wet", "cc_treatment", "drying_treatment")) %>%
  mutate("filtered-run4_n" = mean_mean_n.x - mean_mean_n.y,
         "filtered-run4_c" = mean_mean_c.x - mean_mean_c.y)




## RATIOS

files_ratio <- dir_ls(path = "data/raw_data/EA_CN/2022/",
                        recurse = 1,
                        regex = "\\w+_Run\\d_(repeat_)*(\\d{2}_)*ratio.(xls|XLS)")

run4_ratio <- str_subset(files_ratio, "Run4")
files_filtered_ratio <- setdiff(files_ratio, run4)

source("code/functions/ea_functions/clean_ea_data.R")

clean_run4_ratio <- clean_ea_data(run4_ratio)
clean_run4_ratio$sample_no <- as.numeric(str_sub(
  clean_run4_ratio$sample_no, start = -3))

clean_filtered_ratio <- clean_ea_data(files_filtered_ratio)
clean_filtered_ratio$sample_no <- as.numeric(str_sub(
  clean_filtered_ratio$sample_no, start = -3))

mapped_run4_ratio <- clean_run4_ratio %>%
  group_by(sample_no) %>%
  summarize(mean_cn = mean(c_n_ratio),
            sd_cn = sd(c_n_ratio)) %>%
  left_join(all_treatments) %>%
  group_by(pre_post_wet, cc_treatment, drying_treatment) %>%
  summarize(mean_mean_cn = mean(mean_cn),
            sd_mean_cn = sd(mean_cn))

mapped_filtered_ratio <- clean_filtered_ratio %>%
  group_by(sample_no) %>%
  summarize(mean_cn = mean(c_n_ratio),
            sd_cn = sd(c_n_ratio)) %>%
  left_join(all_treatments) %>%
  group_by(pre_post_wet, cc_treatment, drying_treatment) %>%
  summarize(mean_mean_cn = mean(mean_cn),
            sd_mean_cn = sd(mean_cn))

filtered_v_run4_ratio <- mapped_filtered_ratio %>%
  full_join(mapped_run4_ratio, by=c("pre_post_wet", "cc_treatment", "drying_treatment")) %>%
  mutate("filtered-run4_cn" = mean_mean_cn.x - mean_mean_cn.y)


mapped_filtered_ratio %>%
  filter(pre_post_wet != "no_soil") %>%
  group_by(pre_post_wet, cc_treatment, drying_treatment) %>%
  ggplot(aes(x = pre_post_wet)) +
  geom_col(aes(y = mean_mean_cn,
              fill = cc_treatment),
          position = position_dodge()) +
  geom_errorbar(aes(ymax = mean_mean_cn + sd_mean_cn,
                   ymin = mean_mean_cn - sd_mean_cn,
                   group = cc_treatment),
               position = position_dodge()) +
  coord_cartesian(ylim = c(15, 23)) +
  facet_grid(. ~ factor(drying_treatment,
                       levels = c("initial", "one_wk", "two_wk",
                                  "four_wk", "all_dry"))) +
  scale_x_discrete(limits = c("all_dry","initial", "cw", "pre", "post"))

mapped_run4_ratio <- mapped_run4_ratio %>%
  filter(pre_post_wet != "no_soil") %>%
  group_by(pre_post_wet, cc_treatment, drying_treatment)

mapped_filtered_ratio <- mapped_filtered_ratio %>%
  filter(pre_post_wet != "no_soil") %>%
  group_by(pre_post_wet, cc_treatment, drying_treatment)

ggplot(NULL, aes(x = pre_post_wet,
                 y = mean_mean_cn,
                 fill = cc_treatment)) +
  geom_col(data = mapped_run4_ratio, alpha = 0.5,
           position = position_dodge(), color = "green") +
  geom_col(data = mapped_filtered_ratio, alpha = 0.25,
           position = position_dodge()) +
  geom_errorbar(data = mapped_filtered_ratio,
                aes(ymax = mean_mean_cn + sd_mean_cn,
                    ymin = mean_mean_cn - sd_mean_cn,
                    group = cc_treatment),
                position = position_dodge()) +
  geom_errorbar(data = mapped_run4_ratio,
                aes(ymax = mean_mean_cn + sd_mean_cn,
                    ymin = mean_mean_cn - sd_mean_cn,
                    group = cc_treatment),
                position = position_dodge(), color = "green") +
  coord_cartesian(ylim = c(15, 23)) +
  facet_grid(. ~ factor(drying_treatment,
                        levels = c("initial", "one_wk", "two_wk",
                                   "four_wk", "all_dry"))) +
  scale_x_discrete(limits = c("all_dry","initial", "cw", "pre", "post"))


