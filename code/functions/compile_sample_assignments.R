# This function compiles all the sampling schedules and jar assignments csvs
# that were very horrendously created in the Spring of 2021

# Note that future experiments should have this better formatted. This
# has been hardcoded for Spring 2021's poor planning and may require
# an update or even archiving of this function.

# Sarah Gao
# January 5, 2022
# hellosarahgao@gmail.com

# Load libraries
library("readr")
library("dplyr")

# Read in sampling schedule assignments
compile_sample_assignments <- function() {

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
  return(all_treatments)
}
