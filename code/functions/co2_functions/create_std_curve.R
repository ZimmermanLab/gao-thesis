# This function creates a standard curve to use to calibrate CO2
# readings from the LICOR.

# This is modified from Jacob Anderson's peak.gas package:
# https://github.com/andersonjake1988/peak.gas

# Sarah Gao
# October 29, 2022
# hellosarahgao@gmail.com

library("stringr")
library("tidyr")

create_std_curve <- function(auc_summary_file) {
  # Read in AUCs
  all_auc <- read_csv(auc_summary_file)
  # Clean up sample names
  all_auc_clean <- all_auc %>%
    mutate(sample_no = case_when(
      str_detect(sample, "[0-9]{3}_[0-9]{2}") == TRUE ~
        str_sub(sample, end = 3),
      str_detect(sample, "100ppm") == TRUE ~ "100ppm",
      str_detect(sample, "1000ppm") == TRUE ~ "1000ppm",
      str_detect(sample, "10000ppm") == TRUE ~ "10000ppm",
      str_detect(sample, "100000ppm") == TRUE ~ "100000ppm",
      str_detect(sample, "99per") == TRUE ~ "1000000ppm")) %>%
    filter(!is.na(sample)) %>%
    select(-sample) %>%
    relocate(sample_no, total_auc)
  # Select CO2 standards only
  stds_only <- all_auc_clean %>%
    filter(str_detect(sample_no, "ppm") == TRUE) %>%
    mutate(sample_no = str_sub(sample_no, end = -4))
  stds_only$sample_no <- as.double(stds_only$sample_no)


  # Create a standard curve
  # Set up empty dataframe
  curve <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(curve) <- c("date", "y_intercept", "slope", "r_squared")

  stds_log <- stds_only
  stds_log$date <- as.character(stds_log$date)
  check_alpha <- 0.05
  output_all <- all_auc_clean
  output <- all_auc_clean

  # This creates a different curve for each day and uses that to calibrate
  # sample data by day
    q <- 0
    stds_log <- mutate(stds_log, low = NA, high = NA)
    # For every date (across all standards)
    for(i in 1:length(unique(stds_log$date))){
      # Does each standard by date, so goes through all standards run each day
      date_1 <- filter(stds_log, date == unique(stds_log$date)[i])
      # Creates standard curve for that date
      linmod <- lm(log(total_auc) ~ log(sample_no), data = date_1)
      # For every instance of standard (across all standards) run in a single day
      for(j in 1:nrow(date_1)){
        q <- q+1
        date_new <- date_1[j,]
        # Maps each standard replicate to log curve and sets highs and lows
        # according to a 95% CI (check.alpha = 0.05)
        stds_log$low[q] <- predict(linmod, new_data = date_new,
                                   interval = 'confidence',
                                   level = 1-check_alpha)[2]
        stds_log$high[q] <- predict(linmod, new_data = date_new,
                                    interval = 'confidence',
                                    level = 1-check_alpha)[3]
      }
    curves_summary <- summary(linmod)
    y_int <- curves_summary$coefficients[1]
    slope <- curves_summary$coefficients[2]
    r_sqr <- curves_summary$r.squared
    # Creates dataset that lists the curve generated per date
    curve[i,] <- c(unique(as.character(stds_log$date))[i], y_int, slope, r_sqr)
    }
    # Keeps order of the dates
    preserve.order <- unique(stds_log$date)

    # This checks against standards to see if it's within 95% CI
    check.stand = "TRUE"
    ci.meth = "avg"
      if(check.stand == TRUE){
        ot1 <- output.1 %>%
          group_by(date, sample_no) %>%
          summarize(ci.low = mean(low), ci.high = mean(high))
        if(ci.meth == "avg"){
          ot2 <- output.1 %>%
            group_by(date, sample_no) %>%
            summarize(mean.AUC = mean(log(total_auc)))
          ot3 <- full_join(ot1, ot2, by = c("date", "sample_no"))


          ot3 <- arrange(ot3, date)
          ot3$sample_no <- as.character(ot3$sample_no)
          ot4 <- mutate(ot3, checkci = mean.AUC > ci.low & mean.AUC < ci.high)
          cierr <- data.frame(File_Name = NA, Standard = NA, ci.low = NA,
                              ci.high = NA, mean.AUC = NA)
          naerr <- data.frame(File_Name = NA, Standard = NA, mean.AUC = NA)
       # Haven't checked through this section yet
           } else if(ci.meth == "indiv"){
          ot2 <- select(output.2, File_Name, Sample, Order_Run, Standard, AUC)
          ot2$AUC <- log(ot2$AUC)
          ot3 <- full_join(ot1, ot2, by = c("File_Name", "Standard"))
          ot3$File_Name <- factor(ot3$File_Name, levels = preserve.order)
          ot3 <- arrange(ot3, File_Name, Order_Run)
          ot3$File_Name <- as.character(ot3$File_Name)
          ot4 <- mutate(ot3, checkci = AUC > ci.low & AUC < ci.high)
          cierr <- data.frame(File_Name = NA, Sample = NA, Order_Run = NA, ci.low = NA, ci.high = NA, AUC = NA)
          naerr <- data.frame(File_Name = NA, Sample = NA, Order_Run = NA, AUC = NA)
           }
        #

        if(any(ot4$checkci == FALSE) | any(is.na(ot4$checkci))){
          for(i in 1:nrow(ot4)){
            if(ci.meth == "avg"){
              # Collects NAs from the confidence interval check
              if(is.na(ot4['checkci'][i,])){
                naerr[i,] <- select(ot4[i,], date, sample_no, mean.AUC)
              } else if(ot4['checkci'][i,] == FALSE){
                # Collects samples from the confidence interval check that were
                # out of range
                cierr[i,] <- select(ot4[i,], date, sample_no, ci.low, ci.high, mean.AUC)
              } else if(ot4['checkci'][i,] == TRUE){
                next
              }
            }
          }
          cierr <- na.omit(cierr)
          naerr <- na.omit(naerr)
          if(nrow(cierr) != 0){
            # Prints warning to show which standard samples fall outside of CI
            warning(call. = FALSE, c(
              "\n\nCheck standards deviate from the ", 100*(1 - check.alpha),
              "%", " confidence interval in the following Samples:\n"))
            for(i in 1:nrow(cierr)){
              if(ci.meth == "avg"){
                warning(call. = FALSE, c("File: ", cierr$File_Name[i],
                                         "\tStandard: ", cierr$Standard[i],
                                         "check",
                                         "\tCI range: ",
                                         round(cierr$ci.low[i],2),
                                         " to ", round(cierr$ci.high[i],2),
                                         "\tAUC: ", round(cierr$mean.AUC[i], 2)))
              }
            }
          }
        }
      }
  # Filter out these weirdo standards
  # In pooled:
    curve[, 2:4] <- sapply(curve[, 2:4], as.numeric)
    output_all$date <- as.character(output_all$date)

    # Calculates number of all samples per day
    samples_per_day <- output_all %>%
      group_by(date) %>%
      summarize(n_all = n())
    # Calculates number of all standards per day
    stds_per_day <- stds_only %>%
      group_by(date) %>%
      summarize(n_stds = n())
    # Checks to see if there were any non-standards in each date
    # "samples_run" = FALSE if there were no samples run that day, TRUE if
    # there were samples run that day
    samples_run <- mutate(samples_per_day, n2 = stds_per_day$n_stds,
                      "samples_run" = (n_all != n2))
    samples_curve <- data.frame(matrix(ncol = 4, nrow = 0))
    colnames(samples_curve) <- c("date", "y_intercept", "slope", "r_squared")
    # Maps each  date's standard curve if there samples run on that day
    for(i in 1:length(samples_run$date)){
      for(j in 1:length(curve$date)){
        if(samples_run$date[i] == curve$date[j] &
           samples_run$samples_run[i] == TRUE){
          samples_curve[i,] <- curve[j,]
        } else if(samples_run$samples_run[i] == FALSE){
          samples_curve[i,] <- c(samples_run$date[i], rep(NA,3))
        }
      }
    }
    samples_curve <- arrange(samples_curve, date)
    curve.3 <- fill(samples_curve, "date", "y_intercept", "slope", "r_squared",
                    .direction = "down")
    curve.3[, 2:4] <- sapply(curve.3[, 2:4], as.numeric)
    # Summarizes each standard on each day
    stand <- output.1 %>%
      group_by(date, sample_no) %>%
      summarise(Mean = mean(total_auc), std.dev = sd(total_auc))%>%
      mutate(COV = std.dev/Mean)
    stand$date <- as.character(stand$date)
    output$date <- as.character(output$date)
    sum.stat <- left_join(stand, curve, by = "date")

    # Applies calibration to samples' AUC
    output$auc_calib <- 0
    for(i in 1:nrow(output)){
      for(j in 1:nrow(curve.3)){
        if(output$date[i] == curve.3$date[j]){
          output$auc_calib[i] <- exp((log(output$total_auc[i]) -
                                       curve.3$Y.intercept[j])/
                                      (curve.3$Slope[j]))
        }
      }
    }


    # Create summary
    summary.stats <- output %>%
      filter(is.na(auc_calib) == FALSE) %>%
      group_by(sample_no, date) %>%
      summarise(mean_calib = mean(auc_calib),
                sd_calib = sd(auc_calib),
                median_calib = median(auc_calib),
                iqr_calib = IQR(auc_calib))

    # SAVE THESE OUT
    write_csv(output, "output/2022/co2/auc_calib_date")
    write_csv(summary.stats, "output/2022/co2/auc_sum_calib_date")
}
