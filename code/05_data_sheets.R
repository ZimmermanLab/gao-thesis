# Sarah Gao
# April 9, 2021
# This script creates reusable data sheets based on weekly progress within
# the experiment.

library("dplyr")

# Creates a data sheet for recording constantly watered jars
# Data sheet pre 1 week sampling (i.e. use during the first week)
all_cw_template <- read.csv("output/data_sheets/cw_masses_template.csv")

cw_all <- read.csv("output/schedules/watering_schedule.csv") %>%
  select(jars_cw)
initial_sample_cw_all <- read.csv(
  "output/schedules/initial_sample_cw_all.csv") %>%
  select(jar_no)

cw_wk1 <- subset(
  all_cw_template, !(jars_cw_no %in% initial_sample_cw_all$jar_no))
write.csv(cw_wk1, "output/data_sheets/cw_wk1.csv", row.names = FALSE)

# Data sheet pre 2 week sampling (i.e. use during the second week)
all_1wk_prewet <- read.csv("output/schedules/all_1wk_prewet.csv") %>%
  select(jar_no)

cw_wk2 <- subset(
  cw_wk1, !(jars_cw_no %in% all_1wk_prewet$jar_no))
write.csv(cw_wk2, "output/data_sheets/cw_wk2.csv", row.names = FALSE)

# Data sheet pre 4 week sampling (i.e. use during the third and fourth weeks)
all_2wk_prewet <- read.csv("output/schedules/all_2wk_prewet.csv") %>%
  select(jar_no)

cw_wk34 <- subset(
  cw_wk2, !(jars_cw_no %in% all_2wk_prewet$jar_no))
write.csv(cw_wk34, "output/data_sheets/cw_wk34.csv", row.names = FALSE)
