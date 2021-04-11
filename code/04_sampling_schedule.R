# Sarah Gao
# April 6, 2021
# This script takes in the random treatment assignments and outputs
# sampling schedules to sample random jars within each treatment group.


library("dplyr")

# Load all jar assignments
no_soil_controls <- read.csv("output/jar_assignments/no_soil_controls.csv") %>%
  rename(jar_no = no_soil_controls) %>%
  select(jar_no)
w_cc <- read.csv("output/jar_assignments/w_cc.csv") %>%
  rename(jar_no = jar_w_cc) %>%
  select(jar_no)
no_cc <- read.csv("output/jar_assignments/no_cc.csv") %>%
  rename(jar_no = jar_no_cc) %>%
  select(jar_no)

w_cc_cw <- read.csv(file = "output/jar_assignments/w_cc_cw.csv") %>%
  rename(jar_no = jar_w_cc_cw) %>%
  select(jar_no)
no_cc_cw <- read.csv(file = "output/jar_assignments/no_cc_cw.csv") %>%
  rename(jar_no = jar_no_cc_cw) %>%
  select(jar_no)

# Select 10 random jars for initial sampling
# Commenting out for now, because I fucked up and accidentally reran
# the script and reselecting numbers after having already sampled. Fuck.
''' initial_sample_cw_w_cc <- sample_n(w_cc_cw, 5, replace = FALSE)
initial_sample_cw_no_cc <- sample_n(no_cc_cw, 5, replace = FALSE)
'''

# Already sampled jars (remove on next run):
initial_sample_cw_w_cc <- data.frame(jar_no = c(
  48, 57, 77, 82, 94))
initial_sample_cw_no_cc <- data.frame(jar_no = c(
  27, 39, 71, 100, 106))

initial_sample_cw_all <- initial_sample_cw_w_cc %>%
  rbind(initial_sample_cw_no_cc) %>%
  select(jar_no) %>%
  arrange(jar_no)

write.csv(
  initial_sample_cw_all, file = "output/schedules/initial_sample_cw_all.csv")

# Select 20 random jars at 1 week (prewet)
# Choose 10 consistent watering jars
wk1_sample_cw_w_cc <- select(sample_n(subset(
  w_cc_cw, !(jar_no %in% initial_sample_cw_w_cc$jar_no)), 5, replace = FALSE),
  jar_no)
wk1_sample_cw_no_cc <- select(sample_n(subset(
  no_cc_cw, !(jar_no %in% initial_sample_cw_no_cc$jar_no)), 5, replace = FALSE),
  jar_no)

# Choose 10 pre-wet 1 week drought jars
w_cc_1wk <- read.csv(file = "output/jar_assignments/w_cc_1wk.csv") %>%
  rename(jar_no = jar_w_cc_1wk) %>%
  select(jar_no)
no_cc_1wk <- read.csv(file = "output/jar_assignments/no_cc_1wk.csv") %>%
  rename(jar_no = jar_no_cc_1wk) %>%
  select(jar_no)

wk1_sample_prewet_w_cc <- sample_n(w_cc_1wk, 5, replace = FALSE)
wk1_sample_prewet_no_cc <- sample_n(no_cc_1wk, 5, replace = FALSE)

all_1wk_prewet <- wk1_sample_prewet_w_cc %>%
  rbind(wk1_sample_prewet_no_cc) %>%
  rbind(wk1_sample_cw_no_cc) %>%
  rbind(wk1_sample_cw_w_cc) %>%
  arrange(jar_no)

# Save out 1 week pre-wet sampling list
write.csv(all_1wk_prewet, file = "output/schedules/all_1wk_prewet.csv")

# Choose 10 post-wet 1 week drought jars
wk1_sample_postwet_w_cc <- sample_n(subset(
  w_cc_1wk, !(jar_no %in% all_1wk_prewet$jar_no)), 5, replace = FALSE)

wk1_sample_postwet_no_cc <- sample_n(subset(
  no_cc_1wk, !(jar_no %in% all_1wk_prewet$jar_no)), 5, replace = FALSE)

all_1wk_postwet <- wk1_sample_postwet_w_cc %>%
  rbind(wk1_sample_postwet_no_cc) %>%
  arrange(jar_no)

# Save out 1 week post-wet sampling list
write.csv(all_1wk_postwet, file = "output/schedules/all_1wk_postwet.csv")

# Select 20 random jars at 2 weeks (prewet)
# Choose 10 consistent watering jars
wk2_sample_cw_w_cc <- sample_n(subset(
  w_cc_cw, !((jar_no %in% initial_sample_cw_w_cc$jar_no) |
    (jar_no %in% wk1_sample_cw_w_cc$jar_no))), 5, replace = FALSE)
wk2_sample_cw_no_cc <- sample_n(subset(
  no_cc_cw, !((jar_no %in% initial_sample_cw_no_cc$jar_no) |
    (jar_no %in% wk1_sample_cw_no_cc$jar_no))), 5, replace = FALSE)

# Choose 10 pre-wet 2 week drought jars
w_cc_2wk <- read.csv(file = "output/jar_assignments/w_cc_2wk.csv") %>%
  rename(jar_no = jar_w_cc_2wk) %>%
  select(jar_no)
no_cc_2wk <- read.csv(file = "output/jar_assignments/no_cc_2wk.csv") %>%
  rename(jar_no = jar_no_cc_2wk) %>%
  select(jar_no)

wk2_sample_prewet_w_cc <- sample_n(w_cc_2wk, 5, replace = FALSE)
wk2_sample_prewet_no_cc <- sample_n(no_cc_2wk, 5, replace = FALSE)

all_2wk_prewet <- wk2_sample_prewet_w_cc %>%
  rbind(wk2_sample_prewet_no_cc) %>%
  rbind(wk2_sample_cw_no_cc) %>%
  rbind(wk2_sample_cw_w_cc) %>%
  arrange(jar_no)

# Save out 2 week pre-wet sampling list
write.csv(all_2wk_prewet, file = "output/schedules/all_2wk_prewet.csv")

# Choose 10 post-wet 2 week drought jars
wk2_sample_postwet_w_cc <- sample_n(subset(
  w_cc_2wk, !(jar_no %in% wk2_sample_prewet_w_cc$jar_no)), 5, replace = FALSE)
wk2_sample_postwet_no_cc <- sample_n(subset(
  no_cc_2wk, !(jar_no %in% wk2_sample_prewet_no_cc$jar_no)), 5, replace = FALSE)

all_2wk_postwet <- wk2_sample_postwet_w_cc %>%
  rbind(wk2_sample_postwet_no_cc) %>%
  arrange(jar_no)

# Save out 2 week post-wet sampling list
write.csv(all_2wk_postwet, file = "output/schedules/all_2wk_postwet.csv")

# Select 20 random jars at 4 weeks (prewet)
# Choose 10 consistent watering jars
wk4_sample_cw_w_cc <- sample_n(subset(
  w_cc_cw, !((jar_no %in% initial_sample_cw_w_cc$jar_no) |
               (jar_no %in% wk1_sample_cw_w_cc$jar_no) |
               (jar_no %in% wk2_sample_cw_w_cc$jar_no))), 5, replace = FALSE)
wk4_sample_cw_no_cc <- sample_n(subset(
  no_cc_cw, !((jar_no %in% initial_sample_cw_no_cc$jar_no) |
                (jar_no %in% wk1_sample_cw_no_cc$jar_no) |
                (jar_no %in% wk2_sample_cw_no_cc$jar_no))), 5, replace = FALSE)

# Choose 10 pre-wet 4 week drought jars
w_cc_4wk <- read.csv(file = "output/jar_assignments/w_cc_4wk.csv") %>%
  rename(jar_no = jar_w_cc_4wk) %>%
  select(jar_no)
no_cc_4wk <- read.csv(file = "output/jar_assignments/no_cc_4wk.csv") %>%
  rename(jar_no = jar_no_cc_4wk) %>%
  select(jar_no)

wk4_sample_prewet_w_cc <- sample_n(w_cc_4wk, 5, replace = FALSE)
wk4_sample_prewet_no_cc <- sample_n(no_cc_4wk, 5, replace = FALSE)

all_4wk_prewet <- wk4_sample_prewet_w_cc %>%
  rbind(wk4_sample_prewet_no_cc) %>%
  rbind(wk4_sample_cw_no_cc) %>%
  rbind(wk4_sample_cw_w_cc) %>%
  arrange(jar_no)

# Save out 4 week pre-wet sampling list
write.csv(all_4wk_prewet, file = "output/schedules/all_4wk_prewet.csv")

# Choose 10 post-wet 4 week drought jars
wk4_sample_postwet_w_cc <- sample_n(subset(
  w_cc_4wk, !(jar_no %in% wk4_sample_prewet_w_cc$jar_no)), 5, replace = FALSE)
wk4_sample_postwet_no_cc <- sample_n(subset(
  no_cc_4wk, !(jar_no %in% wk4_sample_prewet_no_cc$jar_no)), 5, replace = FALSE)

all_4wk_postwet <- wk4_sample_postwet_w_cc %>%
  rbind(wk4_sample_postwet_no_cc) %>%
  arrange(jar_no)

# Determine remaining jars that had drought the whole time
wk4_sample_wt_w_cc <- subset(
                             w_cc, !((jar_no %in% all_1wk_prewet$jar_no) |
                             (jar_no %in% all_1wk_postwet$jar_no) |
                             (jar_no %in% all_2wk_prewet$jar_no) |
                             (jar_no %in% all_2wk_postwet$jar_no) |
                             (jar_no %in% all_4wk_prewet$jar_no) |
                             (jar_no %in% all_4wk_postwet$jar_no) |
                             (jar_no %in% initial_sample_cw_all$jar_no) |
                             (jar_no %in% no_soil_controls$jar_no)))
wk4_sample_wt_no_cc <- subset(
                             no_cc, !((jar_no %in% all_1wk_prewet$jar_no) |
                             (jar_no %in% all_1wk_postwet$jar_no) |
                             (jar_no %in% all_2wk_prewet$jar_no) |
                             (jar_no %in% all_2wk_postwet$jar_no) |
                             (jar_no %in% all_4wk_prewet$jar_no) |
                             (jar_no %in% all_4wk_postwet$jar_no) |
                             (jar_no %in% initial_sample_cw_all$jar_no)))

# NOTE: This should be removed upon second run. Only because I failed to save
# out drought assignments in the assignment script.
all_drought_wt <- wk4_sample_wt_w_cc %>%
  rbind(wk4_sample_wt_no_cc) %>%
  select(jar_no) %>%
  arrange(jar_no)
rownames(all_drought_wt) = seq(length= nrow(all_drought_wt))

# Save out all drought jars
write.csv(all_drought_wt, file = "output/schedules/all_drought_wt.csv")

all_4wk_total <- all_4wk_postwet %>%
  rbind(all_drought_wt) %>%
  select(jar_no) %>%
  arrange(jar_no)
rownames(all_4wk_total) = seq(length= nrow(all_4wk_total))

# Save out 4 week post-wet sampling list
write.csv(all_4wk_total, file = "output/schedules/all_4wk_total.csv")
