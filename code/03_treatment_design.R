# Sarah Gao
# April 4, 2021
# This script creates an ordered map of jar locations and randomizes
# treatments to jar numbers.

library("dplyr")

# Note that 9 jars will be used for side measurement of CO2 respiration
# over time which will use a smaller volume of soil but will be sampled at
# similar times. Main experiment requires 115 jars.

all_jars <- data.frame(sample_no = 1:115)

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





# Save out all lists
write.csv(w_cc, file = "output/jar_assignments/w_cc.csv")
write.csv(w_cc_cw, file = "output/jar_assignments/w_cc_cw.csv")
write.csv(w_cc_1wk, file = "output/jar_assignments/w_cc_1wk.csv")
write.csv(w_cc_2wk, file = "output/jar_assignments/w_cc_2wk.csv")
write.csv(w_cc_4wk, file = "output/jar_assignments/w_cc_4wk.csv")
write.csv(w_cc_wt, file = "output/jar_assignments/w_cc_wt.csv")

write.csv(no_cc, file = "output/jar_assignments/no_cc.csv")
write.csv(no_cc_cw, file = "output/jar_assignments/no_cc_cw.csv")
write.csv(no_cc_1wk, file = "output/jar_assignments/no_cc_1wk.csv")
write.csv(no_cc_2wk, file = "output/jar_assignments/no_cc_2wk.csv")
write.csv(no_cc_4wk, file = "output/jar_assignments/no_cc_4wk.csv")
write.csv(no_cc_wt, file = "output/jar_assignments/no_cc_wt.csv")

write.csv(no_soil_controls, file = "output/no_soil_controls.csv")

# Create lists by watering regiment

# By consistent watering
w_cc_cw_blind <- w_cc_cw %>%
  rename(jars_cw = jar_w_cc_cw)
no_cc_cw_blind <- no_cc_cw %>%
  rename(jars_cw = jar_no_cc_cw)

cw_all <- rbind(w_cc_cw_blind, no_cc_cw_blind) %>%
  arrange(jars_cw)

write.csv(cw_all, file = "output/jar_assignments/cw_all.csv")

# By 1 week watering
w_cc_1wk_blind <- w_cc_1wk %>%
  rename(jars_1wk = jar_w_cc_1wk)
no_cc_1wk_blind <- no_cc_1wk %>%
  rename(jars_1wk = jar_no_cc_1wk)

water_1wk <- rbind(w_cc_1wk_blind, no_cc_1wk_blind) %>%
  arrange(jars_1wk)

write.csv(water_1wk, file = "output/jar_assignments/water_1wk.csv")

# By 2 weeks watering
w_cc_2wk_blind <- w_cc_2wk %>%
  rename(jars_2wk = jar_w_cc_2wk)
no_cc_2wk_blind <- no_cc_2wk %>%
  rename(jars_2wk = jar_no_cc_2wk)

water_2wk <- rbind(w_cc_2wk_blind, no_cc_2wk_blind) %>%
  arrange(jars_2wk)

write.csv(water_2wk, file = "output/jar_assignments/water_2wk.csv")

# By 4 weeks watering
w_cc_4wk_blind <- w_cc_4wk %>%
  rename(jars_4wk = jar_w_cc_4wk)
no_cc_4wk_blind <- no_cc_4wk %>%
  rename(jars_4wk = jar_no_cc_4wk)

water_4wk <- rbind(w_cc_4wk_blind, no_cc_4wk_blind) %>%
  arrange(jars_4wk)

write.csv(water_4wk, file = "output/jar_assignments/water_4wk.csv")

# Create master list of watering schedule
water_1wk[nrow(cw_all), ] <- NA
water_2wk[nrow(cw_all), ] <- NA
water_4wk[nrow(cw_all), ] <- NA
water_schedule <- cbind(cw_all, water_1wk, water_2wk, water_4wk)

write.csv(water_schedule, file = "output/watering_schedule.csv")
