library("tidyverse")

# nh3 figure with errorbars
n_data_clean %>%
  mutate(no3 = no2_no3 - no2) %>%
  group_by(sample_no) %>%
  summarize(mean_nh3 = mean(nh3),
            sd_nh3 = sd(nh3),
            mean_no2 = mean(no2),
            sd_no2 = sd(no2),
            mean_no2_no3 = mean(no2_no3),
            sd_no2_no3 = sd(no2_no3)) %>%
  left_join(all_treatments) %>%
  group_by(pre_post_wet, cc_treatment) %>%
  summarize(mean_mean_nh3 = mean(mean_nh3),
            sd_mean_nh3 = sd(mean_nh3)) %>%
  ggplot(aes(x = pre_post_wet,
             y = mean_mean_nh3,
             fill = cc_treatment)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(aes(ymax = mean_mean_nh3 + sd_mean_nh3,
                    ymin = mean_mean_nh3 - sd_mean_nh3),
                position = position_dodge())

# nh3 stats
n_data_clean %>%
  mutate(no3 = no2_no3 - no2) %>%
  group_by(sample_no) %>%
  summarize(mean_nh3 = mean(nh3),
            sd_nh3 = sd(nh3),
            mean_no2 = mean(no2),
            sd_no2 = sd(no2),
            mean_no2_no3 = mean(no2_no3),
            sd_no2_no3 = sd(no2_no3)) %>%
  left_join(all_treatments) %>%
  filter(pre_post_wet != "no_soil") %>%
  filter(pre_post_wet != "cw") %>%
  lm(data = ., mean_nh3 ~ pre_post_wet * cc_treatment) %>%
  anova()

# no2 figure with errorbars
n_data_clean %>%
  mutate(no3 = no2_no3 - no2) %>%
  group_by(sample_no) %>%
  summarize(mean_nh3 = mean(nh3),
            sd_nh3 = sd(nh3),
            mean_no2 = mean(no2),
            sd_no2 = sd(no2),
            mean_no2_no3 = mean(no2_no3),
            sd_no2_no3 = sd(no2_no3)) %>%
  left_join(all_treatments) %>%
  group_by(pre_post_wet, cc_treatment) %>%
  summarize(mean_mean_no2 = mean(mean_no2),
            sd_mean_no2 = sd(mean_no2)) %>%
  ggplot(aes(x = pre_post_wet,
             y = mean_mean_no2,
             fill = cc_treatment)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(aes(ymax = mean_mean_no2 + sd_mean_no2,
                    ymin = mean_mean_no2 - sd_mean_no2),
                position = position_dodge())

# n02 stats
n_data_clean %>%
  mutate(no3 = no2_no3 - no2) %>%
  group_by(sample_no) %>%
  summarize(mean_nh3 = mean(nh3),
            sd_nh3 = sd(nh3),
            mean_no2 = mean(no2),
            sd_no2 = sd(no2),
            mean_no2_no3 = mean(no2_no3),
            sd_no2_no3 = sd(no2_no3)) %>%
  left_join(all_treatments) %>%
  filter(pre_post_wet != "no_soil") %>%
  filter(pre_post_wet != "cw") %>%
  lm(data = ., mean_no2 ~ pre_post_wet * cc_treatment) %>%
  anova()

# no3 figure with errorbars
n_data_clean %>%
  mutate(no3 = no2_no3 - no2) %>%
  group_by(sample_no) %>%
  summarize(mean_nh3 = mean(nh3),
            sd_nh3 = sd(nh3),
            mean_no2 = mean(no2),
            sd_no2 = sd(no2),
            mean_no2_no3 = mean(no2_no3),
            sd_no2_no3 = sd(no2_no3),
            mean_no3 = mean(no3),
            sd_no3 = sd(no3)) %>%
  left_join(all_treatments) %>%
  group_by(pre_post_wet, cc_treatment) %>%
  summarize(mean_mean_no3 = mean(mean_no3),
            sd_mean_no3 = sd(mean_no3)) %>%
  ggplot(aes(x = pre_post_wet,
             y = mean_mean_no3,
             fill = cc_treatment)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(aes(ymax = mean_mean_no3 + sd_mean_no3,
                    ymin = mean_mean_no3 - sd_mean_no3),
                position = position_dodge())

# n03 stats
n_data_clean %>%
  mutate(no3 = no2_no3 - no2) %>%
  group_by(sample_no) %>%
  summarize(mean_nh3 = mean(nh3),
            sd_nh3 = sd(nh3),
            mean_no2 = mean(no2),
            sd_no2 = sd(no2),
            mean_no2_no3 = mean(no2_no3),
            sd_no2_no3 = sd(no2_no3),
            mean_no3 = mean(no3),
            sd_no3 = sd(no3)) %>%
  left_join(all_treatments) %>%
  filter(pre_post_wet != "no_soil") %>%
  filter(pre_post_wet != "cw") %>%
  lm(data = ., mean_no3 ~ pre_post_wet * cc_treatment) %>%
  anova()
