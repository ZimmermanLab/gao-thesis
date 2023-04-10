# This is a quick script to see if dirty blanks in bacterial qPCR runs
# affect results significantly

# Sarah Gao
# October 18, 2022
# hellosarahgao@gmail.com

run1_clean <- read_csv(
  "data/raw_data/qPCR/20221014/2022-10-14_Run1_bacterial -  Quantification Cq Results.csv") %>%
  filter(Fluor == "SYBR") %>%
  select(Sample, Cq) %>%
  filter(!(Sample == "Blank")) %>%
  rename("sample_no_full" = Sample, "cq" = Cq) %>%
  mutate("sample_no" = as.numeric(str_sub(sample_no_full, end = 3))) %>%
  mutate("rep_no" = as.numeric(str_sub(sample_no_full, start = -1))) %>%
  filter(!(is.na(cq))) %>%
  filter(cq < 16)

run2_dirty_1 <- read_csv("data/raw_data/qPCR/20221017/2022-10-17_Run2_bacterial -  Quantification Cq Results.csv") %>%
  filter(Fluor == "SYBR") %>%
  select(Sample, Cq) %>%
  filter(!(Sample == "Blank")) %>%
  rename("sample_no_full" = Sample, "cq" = Cq) %>%
  mutate("sample_no" = as.numeric(str_sub(sample_no_full, end = 3))) %>%
  mutate("rep_no" = as.numeric(str_sub(sample_no_full, start = -1))) %>%
  filter(cq < 20)

run2_dirty_2 <- read_csv("data/raw_data/qPCR/20221018/2022-10-18_Run2_02_bacterial.csv") %>%
  slice(-(1:19)) %>%
  rename("Sample" = 5, "Fluor" = 2, "Cq" = 6) %>%
  filter(Fluor == "SYBR") %>%
  select(Sample, Cq) %>%
  filter(!(str_detect(Sample, "Blank"))) %>%
  rename("sample_no_full" = Sample, "cq" = Cq) %>%
  mutate("sample_no" = as.numeric(str_sub(sample_no_full, end = 3))) %>%
  mutate("rep_no" = as.numeric(str_sub(sample_no_full, start = -1))) %>%
  filter(sample_no != 3)
run2_dirty_2$cq <- as.numeric(run2_dirty_2$cq)

run3_dirty_1 <- read_csv("data/raw_data/qPCR/20221017/2022-10-17_Run3_bacterial -  Quantification Cq Results.csv") %>%
  filter(Fluor == "SYBR") %>%
  select(Sample, Cq) %>%
  filter(!(Sample == "Blank")) %>%
  rename("sample_no_full" = Sample, "cq" = Cq) %>%
  mutate("sample_no" = as.numeric(str_sub(sample_no_full, end = 3))) %>%
  mutate("rep_no" = as.numeric(str_sub(sample_no_full, start = -1)))


all_treatments <- readr::read_csv("output/2022/jar_assignments/master_list.csv")

mapped_clean <- run1_clean %>%
  group_by(sample_no) %>%
  summarize(mean_cq = mean(cq),
            sd_cq = sd(cq)) %>%
  left_join(all_treatments) %>%
  group_by(pre_post_wet, cc_treatment, drying_treatment) %>%
  summarize(mean_mean_cq = mean(mean_cq),
            sd_mean_cq = sd(mean_cq))

mapped_dirty_1 <- run2_dirty_1 %>%
  group_by(sample_no) %>%
  summarize(mean_cq = mean(cq),
            sd_cq = sd(cq)) %>%
  left_join(all_treatments) %>%
  group_by(pre_post_wet, cc_treatment, drying_treatment) %>%
  summarize(mean_mean_cq = mean(mean_cq),
            sd_mean_cq = sd(mean_cq))

mapped_dirty_2 <- run2_dirty_2 %>%
  group_by(sample_no) %>%
  summarize(mean_cq = mean(cq),
            sd_cq = sd(cq)) %>%
  left_join(all_treatments) %>%
  group_by(pre_post_wet, cc_treatment, drying_treatment) %>%
  summarize(mean_mean_cq = mean(mean_cq),
            sd_mean_cq = sd(mean_cq))

mapped_dirty_3 <- run3_dirty_1 %>%
  group_by(sample_no) %>%
  summarize(mean_cq = mean(cq),
            sd_cq = sd(cq)) %>%
  left_join(all_treatments) %>%
  group_by(pre_post_wet, cc_treatment, drying_treatment) %>%
  summarize(mean_mean_cq = mean(mean_cq),
            sd_mean_cq = sd(mean_cq))

run1_clean_stats <- run1_clean %>%
  select(sample_no, cq) %>%
  filter(cq < 16) %>%
  group_by(sample_no) %>%
  summarize(mean_cq = mean(cq), sd_cq = sd(cq))
sd_clean <- sd(run1_clean_stats$mean_cq)

run2_dirty1_stats <- run2_dirty_1 %>%
  select(sample_no, cq) %>%
  filter(cq < 20) %>%
  group_by(sample_no) %>%
  summarize(mean_cq = mean(cq), sd_cq = sd(cq))
sd_dirty1 <- sd(run2_dirty1_stats$mean_cq)

clean_v_dirty1 <- mapped_clean %>%
  full_join(mapped_dirty_1, by=c("pre_post_wet", "cc_treatment", "drying_treatment")) %>%
  mutate("clean-dirty" = mean_mean_cq.x - mean_mean_cq.y)
clean_v_dirty2 <- mapped_clean %>%
  full_join(mapped_dirty_2, by=c("pre_post_wet", "cc_treatment", "drying_treatment")) %>%
  mutate("clean-dirty" = mean_mean_cq.x - mean_mean_cq.y)
clean_v_dirty3 <- mapped_clean %>%
  full_join(mapped_dirty_3, by=c("pre_post_wet", "cc_treatment", "drying_treatment")) %>%
  mutate("clean-dirty" = mean_mean_cq.x - mean_mean_cq.y)

dirty_v_dirty12 <- mapped_dirty_1 %>%
  full_join(mapped_dirty_2, by=c("pre_post_wet", "cc_treatment", "drying_treatment")) %>%
  mutate("dirty1-dirty2" = mean_mean_cq.x - mean_mean_cq.y)
dirty_v_dirty13 <- mapped_dirty_1 %>%
  full_join(mapped_dirty_3, by=c("pre_post_wet", "cc_treatment", "drying_treatment")) %>%
  mutate("dirty1-dirty2" = mean_mean_cq.x - mean_mean_cq.y)
dirty_v_dirty23 <- mapped_dirty_2 %>%
  full_join(mapped_dirty_3, by=c("pre_post_wet", "cc_treatment", "drying_treatment")) %>%
  mutate("dirty1-dirty2" = mean_mean_cq.x - mean_mean_cq.y)



left_join(mapped_dirty, mapped_clean) %>%
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
  coord_cartesian(ylim=c(10, 17))


mapped_clean %>%
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
                position = position_dodge(0.9))

mapped_dirty %>%
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
                position = position_dodge(0.9))
