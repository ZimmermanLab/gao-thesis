# Sarah Gao
# April 4, 2021
# This script creates an ordered map of jar locations and randomizes
# treatments to jar numbers.

library("dplyr")

# Note that 18 jars (dried for 1 week, 2 weeks and 4 weeks) will be used for side measurement of CO2 respiration
# over time which will use a smaller volume of soil.
# Main experiment requires 115 jars.

all_jars <- data.frame(sample_no = 1:133)

assign_jars <- function(group_name, group_num, num_choose, already_chosen) {
  already_chosen <- already_chosen %>%
    select(sample_no)
  assignments <- data.frame(sample_no = NA, group_no = NA)
  assignment_group <- 0
  for (group in 1:group_num) {
    assignment_group <- assignment_group + 1
    chosen_jars <- sample_n(subset(group_name, !(group_name$sample_no %in% already_chosen$sample_no)),
      num_choose, replace = FALSE)
    group_assign <- cbind(chosen_jars, group_no = assignment_group)
    assignments <- rbind(group_assign, assignments)
    already_chosen <- rbind(already_chosen, chosen_jars)
  }
  return(assignments)
}

# Choose jars for w_cc and no_cc
already_chosen <- data.frame(sample_no = NA)
w_cc <- assign_jars(all_jars, 1, 55, already_chosen) %>%
  select(sample_no) %>%
  drop_na() %>%
  arrange(sample_no)
already_chosen <- w_cc
no_cc <- assign_jars(all_jars, 1, 55, already_chosen) %>%
  select(sample_no) %>%
  drop_na() %>%
  arrange(sample_no)

# Choose jars for CO2 experiment
already_chosen <- data.frame(sample_no = NA)
w_cc_co2_num <- assign_jars(w_cc, 3, 3, already_chosen)
w_cc_co2 <- w_cc_co2_num %>%
  drop_na() %>%
  rename(drying_treatment = group_no) %>%
  mutate(drying_treatment = case_when(drying_treatment = 1 ~ "one_wk",
                                      drying_treatment = 2 ~ "two_wk",) %>%
  mutate(pre_post_wet = "co2") %>%
  mutate(cc_treatment = "w_cc") %>%
  arrange(sample_no)
w_cc_

# Choose jars for no soil controls
already_chosen <- rbind(w_cc, no_cc)
no_soil_controls <- assign_jars(all_jars, 1, 5, already_chosen) %>%
  drop_na() %>%
  rename(pre_post_wet = group_no) %>%
  mutate(pre_post_wet = "no_soil") %>%
  mutate(cc_treatment = "no_soil") %>%
  mutate(drying_treatment = "no_soil") %>%
  arrange(sample_no)

# Choose jars for w_cc_initial
already_chosen <- data.frame(sample_no = NA)
w_cc_initial_num <- assign_jars(w_cc, 1, 5, already_chosen)
w_cc_initial <- w_cc_initial_num %>%
  drop_na() %>%
  rename(pre_post_wet = group_no) %>%
  mutate(pre_post_wet = "initial") %>%
  mutate(cc_treatment = "w_cc") %>%
  mutate(drying_treatment = "initial") %>%
  arrange(sample_no)

# Choose jars for w_cc_1wk
already_chosen <- w_cc_initial_num
w_cc_1wk_num <- assign_jars(w_cc, 2, 5, already_chosen)
w_cc_1wk <- w_cc_1wk_num %>%
  drop_na() %>%
  mutate(group_no = case_when(group_no == 1 ~ "pre", group_no == 2 ~ "post")) %>%
  rename(pre_post_wet = group_no) %>%
  mutate(cc_treatment = "w_cc") %>%
  mutate(drying_treatment = "one_wk") %>%
  arrange(sample_no)

# Choose jars for w_cc_2wk
already_chosen <- rbind(w_cc_initial_num, w_cc_1wk_num)
w_cc_2wk_num <- assign_jars(w_cc, 2, 5, already_chosen)
w_cc_2wk <- w_cc_2wk_num %>%
  drop_na() %>%
  mutate(group_no = case_when(group_no == 1 ~ "pre", group_no == 2 ~ "post")) %>%
  rename(pre_post_wet = group_no) %>%
  mutate(cc_treatment = "w_cc") %>%
  mutate(drying_treatment = "two_wk") %>%
  arrange(sample_no)

# Choose jars for w_cc_4wk
already_chosen <- rbind(w_cc_initial_num, w_cc_1wk_num, w_cc_2wk_num)
w_cc_4wk_num <- assign_jars(w_cc, 2, 5, already_chosen)
w_cc_4wk <- w_cc_4wk_num %>%
  drop_na() %>%
  mutate(group_no = case_when(group_no == 1 ~ "pre", group_no == 2 ~ "post")) %>%
  rename(pre_post_wet = group_no) %>%
  mutate(cc_treatment = "w_cc") %>%
  mutate(drying_treatment = "four_wk") %>%
  arrange(sample_no)

# Choose jars for w_cc_cw_1wk
already_chosen <- rbind(w_cc_initial_num, w_cc_1wk_num, w_cc_2wk_num,
                        w_cc_4wk_num)
w_cc_cw_1wk_num <- assign_jars(w_cc, 1, 5, already_chosen)
w_cc_cw_1wk <- w_cc_cw_1wk_num %>%
  drop_na() %>%
  mutate(group_no = "cw") %>%
  rename(pre_post_wet = group_no) %>%
  mutate(cc_treatment = "w_cc") %>%
  mutate(drying_treatment = "one_wk") %>%
  arrange(sample_no)

# Choose jars for w_cc_cw_2wk
already_chosen <- rbind(w_cc_initial_num, w_cc_1wk_num, w_cc_2wk_num,
                        w_cc_4wk_num, w_cc_cw_1wk_num)
w_cc_cw_2wk_num <- assign_jars(w_cc, 1, 5, already_chosen)
w_cc_cw_2wk <- w_cc_cw_2wk_num %>%
  drop_na() %>%
  mutate(group_no = "cw") %>%
  rename(pre_post_wet = group_no) %>%
  mutate(cc_treatment = "w_cc") %>%
  mutate(drying_treatment = "two_wk") %>%
  arrange(sample_no)

# Choose jars for w_cc_cw_4wk
already_chosen <- rbind(w_cc_initial_num, w_cc_1wk_num, w_cc_2wk_num,
                        w_cc_4wk_num, w_cc_cw_1wk_num, w_cc_cw_2wk_num)
w_cc_cw_4wk_num <- assign_jars(w_cc, 1, 5, already_chosen)
w_cc_cw_4wk <- w_cc_cw_4wk_num %>%
  drop_na() %>%
  mutate(group_no = "cw") %>%
  rename(pre_post_wet = group_no) %>%
  mutate(cc_treatment = "w_cc") %>%
  mutate(drying_treatment = "four_wk") %>%
  arrange(sample_no)

# Choose jars for w_cc_all_dry
already_chosen <- rbind(w_cc_initial_num, w_cc_1wk_num, w_cc_2wk_num,
                        w_cc_4wk_num, w_cc_cw_1wk_num, w_cc_cw_2wk_num,
                        w_cc_cw_4wk_num)
w_cc_all_dry_num <- assign_jars(w_cc, 1, 5, already_chosen)
w_cc_all_dry <- w_cc_all_dry_num %>%
  drop_na() %>%
  mutate(group_no = "all_dry") %>%
  rename(pre_post_wet = group_no) %>%
  mutate(cc_treatment = "w_cc") %>%
  mutate(drying_treatment = "all_dry") %>%
  arrange(sample_no)

# Choose jars for no_cc_initial
already_chosen <- data.frame(sample_no = NA)
no_cc_initial_num <- assign_jars(no_cc, 1, 5, already_chosen)
no_cc_initial <- no_cc_initial_num %>%
  drop_na() %>%
  rename(pre_post_wet = group_no) %>%
  mutate(pre_post_wet = "initial") %>%
  mutate(cc_treatment = "no_cc") %>%
  mutate(drying_treatment = "initial") %>%
  arrange(sample_no)

# Choose jars for no_cc_1wk
already_chosen <- no_cc_initial_num
no_cc_1wk_num <- assign_jars(no_cc, 2, 5, already_chosen)
no_cc_1wk <- no_cc_1wk_num %>%
  drop_na() %>%
  mutate(group_no = case_when(group_no == 1 ~ "pre", group_no == 2 ~ "post")) %>%
  rename(pre_post_wet = group_no) %>%
  mutate(cc_treatment = "no_cc") %>%
  mutate(drying_treatment = "one_wk") %>%
  arrange(sample_no)

# Choose jars for no_cc_2wk
already_chosen <- rbind(no_cc_initial_num, no_cc_1wk_num)
no_cc_2wk_num <- assign_jars(no_cc, 2, 5, already_chosen)
no_cc_2wk <- no_cc_2wk_num %>%
  drop_na() %>%
  mutate(group_no = case_when(group_no == 1 ~ "pre", group_no == 2 ~ "post")) %>%
  rename(pre_post_wet = group_no) %>%
  mutate(cc_treatment = "no_cc") %>%
  mutate(drying_treatment = "two_wk") %>%
  arrange(sample_no)

# Choose jars for no_cc_4wk
already_chosen <- rbind(no_cc_initial_num, no_cc_1wk_num, no_cc_2wk_num)
no_cc_4wk_num <- assign_jars(no_cc, 2, 5, already_chosen)
no_cc_4wk <- no_cc_4wk_num %>%
  drop_na() %>%
  mutate(group_no = case_when(group_no == 1 ~ "pre", group_no == 2 ~ "post")) %>%
  rename(pre_post_wet = group_no) %>%
  mutate(cc_treatment = "no_cc") %>%
  mutate(drying_treatment = "four_wk") %>%
  arrange(sample_no)

# Choose jars for no_cc_cw_1wk
already_chosen <- rbind(no_cc_initial_num, no_cc_1wk_num, no_cc_2wk_num,
                        no_cc_4wk_num)
no_cc_cw_1wk_num <- assign_jars(no_cc, 1, 5, already_chosen)
no_cc_cw_1wk <- no_cc_cw_1wk_num %>%
  drop_na() %>%
  mutate(group_no = "cw") %>%
  rename(pre_post_wet = group_no) %>%
  mutate(cc_treatment = "no_cc") %>%
  mutate(drying_treatment = "one_wk") %>%
  arrange(sample_no)

# Choose jars for no_cc_cw_2wk
already_chosen <- rbind(no_cc_initial_num, no_cc_1wk_num, no_cc_2wk_num,
                        no_cc_4wk_num, no_cc_cw_1wk_num)
no_cc_cw_2wk_num <- assign_jars(no_cc, 1, 5, already_chosen)
no_cc_cw_2wk <- no_cc_cw_2wk_num %>%
  drop_na() %>%
  mutate(group_no = "cw") %>%
  rename(pre_post_wet = group_no) %>%
  mutate(cc_treatment = "no_cc") %>%
  mutate(drying_treatment = "two_wk") %>%
  arrange(sample_no)

# Choose jars for no_cc_cw_4wk
already_chosen <- rbind(no_cc_initial_num, no_cc_1wk_num, no_cc_2wk_num,
                        no_cc_4wk_num, no_cc_cw_1wk_num, no_cc_cw_2wk_num)
no_cc_cw_4wk_num <- assign_jars(no_cc, 1, 5, already_chosen)
no_cc_cw_4wk <- no_cc_cw_4wk_num %>%
  drop_na() %>%
  mutate(group_no = "cw") %>%
  rename(pre_post_wet = group_no) %>%
  mutate(cc_treatment = "no_cc") %>%
  mutate(drying_treatment = "four_wk") %>%
  arrange(sample_no)

# Choose jars for no_cc_all_dry
already_chosen <- rbind(no_cc_initial_num, no_cc_1wk_num, no_cc_2wk_num,
                        no_cc_4wk_num, no_cc_cw_1wk_num, no_cc_cw_2wk_num,
                        no_cc_cw_4wk_num)
no_cc_all_dry_num <- assign_jars(no_cc, 1, 5, already_chosen)
no_cc_all_dry <- no_cc_all_dry_num %>%
  drop_na() %>%
  mutate(group_no = "all_dry") %>%
  rename(pre_post_wet = group_no) %>%
  mutate(cc_treatment = "no_cc") %>%
  mutate(drying_treatment = "all_dry") %>%
  arrange(sample_no)

# Combine all into master list
all_jars <- rbind(no_soil_controls,
                  w_cc_initial, w_cc_1wk, w_cc_2wk, w_cc_4wk, w_cc_cw_1wk,
                  w_cc_cw_2wk, w_cc_cw_4wk, w_cc_all_dry,
                  no_cc_initial, no_cc_1wk, no_cc_2wk, no_cc_4wk, no_cc_cw_1wk,
                  no_cc_cw_2wk, no_cc_cw_4wk, no_cc_all_dry) %>%
  arrange(sample_no)

# Save out to csvs
write_csv(all_jars, "output/2022/jar_assignments/master_list.csv")
write_csv(no_cc, "output/2022/jar_assignments/no_cc.csv")
write_csv(w_cc, "output/2022/jar_assignments/w_cc.csv")
write_csv(no_soil_controls, "output/2022/jar_assignments/no_soil_controls.csv")

# Save out initial sampling schedule
initial_samples <- rbind(w_cc_initial, no_cc_initial) %>%
  arrange(sample_no) %>%
  write_csv("output/2022/jar_assignments/initial_sampling.csv")
initial_samples %>% select(sample_no) %>%
  write_csv("output/2022/schedules/initial_samples.csv")

# Save out week 1 pre-wet sampling schedule
wk1_pre <- rbind((w_cc_1wk %>% filter(pre_post_wet == "pre")),
                 (no_cc_1wk %>% filter(pre_post_wet == "pre")),
                 w_cc_cw_1wk, no_cc_cw_1wk) %>%
  arrange(sample_no) %>%
  write_csv("output/2022/jar_assignments/wk1_pre.csv")
wk1_pre %>% select(sample_no) %>%
  write_csv("output/2022/schedules/wk1_pre.csv")

# Save out week 1 post-wet sampling schedule
wk1_post <- rbind((w_cc_1wk %>% filter(pre_post_wet == "post")),
                  (no_cc_1wk %>% filter(pre_post_wet == "post"))) %>%
  arrange(sample_no) %>%
  write_csv("output/2022/jar_assignments/wk1_post.csv")
wk1_post %>% select(sample_no) %>%
  write_csv("output/2022/schedules/wk2_post.csv")

# Save out week 2 pre-wet sampling schedule
wk2_pre <- rbind((w_cc_2wk %>% filter(pre_post_wet == "pre")),
                 (no_cc_2wk %>% filter(pre_post_wet == "pre")),
                 w_cc_cw_2wk, no_cc_cw_2wk) %>%
  arrange(sample_no) %>%
  write_csv("output/2022/jar_assignments/wk2_pre.csv")
wk2_pre %>% select(sample_no) %>%
  write_csv("output/2022/schedules/wk2_pre.csv")

# Save out week 2 post-wet sampling schedule
wk2_post <- rbind((w_cc_2wk %>% filter(pre_post_wet == "post")),
                  (no_cc_2wk %>% filter(pre_post_wet == "post"))) %>%
  arrange(sample_no) %>%
  write_csv("output/2022/jar_assignments/wk2_post.csv")
wk2_post %>% select(sample_no) %>%
  write_csv("output/2022/schedules/wk2_post.csv")

# Save out week 4 pre-wet sampling schedule
wk4_pre <- rbind((w_cc_4wk %>% filter(pre_post_wet == "pre")),
                 (no_cc_4wk %>% filter(pre_post_wet == "pre")),
                 w_cc_cw_4wk, no_cc_cw_4wk) %>%
  arrange(sample_no) %>%
  write_csv("output/2022/jar_assignments/wk4_pre.csv")
wk4_pre %>% select(sample_no) %>%
  write_csv("output/2022/schedules/wk4_pre.csv")

# Save out week 4 post-wet sampling schedule, including the all dry controls
# and no soil controls
wk4_post <- rbind((w_cc_4wk %>% filter(pre_post_wet == "post")),
                  (no_cc_4wk %>% filter(pre_post_wet == "post")),
                  w_cc_all_dry, no_cc_all_dry, no_soil_controls) %>%
  arrange(sample_no) %>%
  write_csv("output/2022/jar_assignments/wk4_post.csv")
wk4_post %>% select(sample_no) %>%
  write_csv("output/2022/schedules/wk4_post_all.csv")
wk4_post %>% filter(pre_post_wet == "post") %>% select(sample_no) %>%
  write_csv("output/2022/schedules/wk4_post_water.csv")

# Create watering schedules for constantly watered samples
wk1_water <- all_jars %>%
  filter(pre_post_wet == "cw") %>%
  select(sample_no) %>%
  write_csv("output/2022/schedules/wk1_cw_water.csv")

wk2_water <- all_jars %>%
  filter(pre_post_wet == "cw" & drying_treatment != "one_wk") %>%
  select(sample_no) %>%
  write_csv("output/2022/schedules/wk2_cw_water.csv")

wk4_water <- all_jars %>%
  filter(pre_post_wet == "cw" & drying_treatment == "four_wk") %>%
  select(sample_no) %>%
  write_csv("output/2022/schedules/wk4_cw_water.csv")
