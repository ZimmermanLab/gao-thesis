# Sarah Gao
# April 4, 2021
# This script creates an ordered map of jar locations and randomizes
# treatments to jar numbers.

library("dplyr")

all_jars_plot <- read.csv("data/exp_design/Experimental_Design.csv")

total_jars <- data.frame(total_jars = 1:115)

# Choose jars for cover crop and no cover crop
num_w_cc <- 55
num_no_cc <- 55

w_cc <- data.frame(jar_w_cc = sample(max(total_jars),
                                     num_w_cc, replace = FALSE)) %>%
  arrange(jar_w_cc)
no_cc <- sample_n(subset(total_jars,
                  !(total_jars %in% w_cc$jar_w_cc)), 55, replace = FALSE) %>%
  rename(jar_no_cc = total_jars) %>%
  arrange(jar_no_cc)

# Choose jars for consistent watering
w_cc_cw <- sample_n(w_cc, 20, replace = FALSE) %>%
  rename(jar_w_cc_cw = jar_w_cc) %>%
  arrange(jar_w_cc_cw)

no_cc_cw <- sample_n(no_cc, 20, replace = FALSE) %>%
  rename(jar_no_cc_cw = jar_no_cc) %>%
  arrange(jar_no_cc_cw)

# Choose jars for 1 week drought
w_cc_1wk <- sample_n(subset(w_cc,
                                !(jar_w_cc %in% w_cc_cw$jar_w_cc_cw)),
                                10, replace = FALSE) %>%
  rename(jar_w_cc_1wk = jar_w_cc) %>%
  arrange(jar_w_cc_1wk)

no_cc_1wk <- sample_n(subset(no_cc,
                                !(jar_no_cc %in% no_cc_cw$jar_no_cc_cw)),
                                10, replace = FALSE) %>%
  rename(jar_no_cc_1wk = jar_no_cc) %>%
  arrange(jar_no_cc_1wk)

# Choose jars for 2 week drought
w_cc_2wk <- sample_n(subset(w_cc,
                                !((jar_w_cc %in% w_cc_cw$jar_w_cc_cw) |
                                (jar_w_cc %in% w_cc_1wk$jar_w_cc_1wk))),
                                10, replace = FALSE) %>%
  rename(jar_w_cc_2wk = jar_w_cc) %>%
  arrange(jar_w_cc_2wk)

no_cc_2wk <- sample_n(subset(no_cc,
                                !((jar_no_cc %in% no_cc_cw$jar_no_cc_cw) |
                                (jar_no_cc %in% no_cc_1wk$jar_no_cc_1wk))),
                                10, replace = FALSE) %>%
    rename(jar_no_cc_2wk = jar_no_cc) %>%
    arrange(jar_no_cc_2wk)

# Choose jars for 4 week drought
w_cc_4wk <- sample_n(subset(w_cc,
                                !((jar_w_cc %in% w_cc_cw$jar_w_cc_cw) |
                                (jar_w_cc %in% w_cc_1wk$jar_w_cc_1wk) |
                                (jar_w_cc %in% w_cc_2wk$jar_w_cc_2wk))),
                                10, replace = FALSE) %>%
  rename(jar_w_cc_4wk = jar_w_cc) %>%
  arrange(jar_w_cc_4wk)

no_cc_4wk <- sample_n(subset(no_cc,
                                !((jar_no_cc %in% no_cc_cw$jar_no_cc_cw) |
                                (jar_no_cc %in% no_cc_1wk$jar_no_cc_1wk) |
                                (jar_no_cc %in% no_cc_2wk$jar_no_cc_2wk))),
                                10, replace = FALSE) %>%
  rename(jar_no_cc_4wk = jar_no_cc) %>%
  arrange(jar_no_cc_4wk)

# Choose jars for drought whole time
w_cc_wt <- sample_n(subset(w_cc,
                                !((jar_w_cc %in% w_cc_cw$jar_w_cc_cw) |
                                (jar_w_cc %in% w_cc_1wk$jar_w_cc_1wk) |
                                (jar_w_cc %in% w_cc_2wk$jar_w_cc_2wk) |
                                (jar_w_cc %in% w_cc_4wk$jar_w_cc_4wk))),
                                10, replace = FALSE) %>%
  rename(jar_w_cc_wt = jar_w_cc) %>%
  arrange(jar_w_cc_wt)

no_cc_wt <- sample_n(subset(no_cc,
                                !((jar_no_cc %in% no_cc_cw$jar_no_cc_cw) |
                                (jar_no_cc %in% no_cc_1wk$jar_no_cc_1wk) |
                                (jar_no_cc %in% no_cc_2wk$jar_no_cc_2wk) |
                                (jar_no_cc %in% no_cc_4wk$jar_no_cc_4wk))),
                                10, replace = FALSE) %>%
  rename(jar_no_cc_wt = jar_no_cc) %>%
  arrange(jar_no_cc_wt)

# Choose jars for 5 controls of no soil, only water
no_soil_controls <- subset(total_jars,
                           !((total_jars %in% w_cc$jar_w_cc) |
                            (total_jars %in% no_cc$jar_no_cc))) %>%
  rename(no_soil_controls = total_jars) %>%
  arrange(no_soil_controls)

# Create lists by watering regiment

# By consistent watering
w_cc_cw_blind <- w_cc_cw %>%
  rename(jars_cw = jar_w_cc_cw)
no_cc_cw_blind <- no_cc_cw %>%
  rename(jars_cw = jar_no_cc_cw)

water_consistent <- rbind(w_cc_cw_blind, no_cc_cw_blind) %>%
  arrange(jars_cw)

# By 1 week watering
w_cc_1wk_blind <- w_cc_1wk %>%
  rename(jars_1wk = jar_w_cc_1wk)
no_cc_1wk_blind <- no_cc_1wk %>%
  rename(jars_1wk = jar_no_cc_1wk)

water_1wk <- rbind(w_cc_1wk_blind, no_cc_1wk_blind) %>%
  arrange(jars_1wk)

# By 2 weeks watering
w_cc_2wk_blind <- w_cc_2wk %>%
  rename(jars_2wk = jar_w_cc_2wk)
no_cc_2wk_blind <- no_cc_2wk %>%
  rename(jars_2wk = jar_no_cc_2wk)

water_2wk <- rbind(w_cc_2wk_blind, no_cc_2wk_blind) %>%
  arrange(jars_2wk)

# By 4 weeks watering
w_cc_4wk_blind <- w_cc_4wk %>%
  rename(jars_4wk = jar_w_cc_4wk)
no_cc_4wk_blind <- no_cc_4wk %>%
  rename(jars_4wk = jar_no_cc_4wk)

water_4wk <- rbind(w_cc_4wk_blind, no_cc_4wk_blind) %>%
  arrange(jars_4wk)

# Create master list of watering schedule
water_1wk[nrow(water_consistent), ] <- NA
water_2wk[nrow(water_consistent), ] <- NA
water_4wk[nrow(water_consistent), ] <- NA
water_schedule <- cbind(water_consistent, water_1wk, water_2wk, water_4wk)

write.csv(water_schedule, file = "output/watering_schedule.csv")
