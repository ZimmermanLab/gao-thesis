# This function creates a standard curve to use to calibrate CO2
# readings from the LICOR.

# This is modified from Jacob Anderson's peak.gas package:
# https://github.com/andersonjake1988/peak.gas

# Sarah Gao
# October 29, 2022
# hellosarahgao@gmail.com

# Read in AUCs
all_auc <- read_csv("output/2022/co2/auc_summary_all.txt")
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
curve <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(curve) <- c("date", "Y.intercept", "Slope", "R.squared")

output.1 <- stds_only
output.1$date <- as.character(output.1$date)
check.alpha <- 0.05
output_all <- all_auc_clean
output <- all_auc_clean

# This pools together the standards across all days and creates one curve
# from that
stds_pooled <- stds_only %>%
  group_by(sample_no) %>%
  mutate(low = NA, high = NA)
# Creates standard curve
linmod_pooled <- lm(log(total_auc) ~ log(sample_no), data = stds_pooled)

r <- 0
# For every instance of standard (across all standards)
for(m in 1:nrow(stds_pooled)) {
  r <- r+1
  new_data <- stds_pooled[m,]
  # Predict with 95% confidence new values of each replicate (across
  # all standards) using linmod_pooled
  stds_pooled$low[m] <- predict(linmod_pooled, newdata = new_data,
                             interval = 'confidence',
                             level = 1-check.alpha)[2]
  stds_pooled$high[m] <- predict(linmod_pooled, newdata = new_data,
                              interval = 'confidence',
                              level = 1-check.alpha)[3]
}
summary_pooled <- summary(linmod_pooled)
sp_Y <- summary_pooled$coefficients[1]
sp_M <- summary_pooled$coefficients[2]
sp_R <- summary_pooled$r.squared
# Create dataframe of curve results
curve_pooled <- data.frame("Y.intercept" = sp_Y, "Slope" = sp_M,
                          "R.squared" = sp_R)
# Applies normalization to samples' AUC
output_all$auc_norm <- 0
for(i in 1:nrow(output_all)){
  output_all$auc_norm[i] <- exp((log(output_all$total_auc[i]) -
                               curve_pooled$Y.intercept)/
                              (curve_pooled$Slope))
}
# Create summary
all_pooled_stats <- output_all %>%
  group_by(sample_no, date) %>%
  summarise(mean_norm = mean(auc_norm),
            sd_norm = sd(auc_norm),
            median_norm = median(auc_norm),
            iqr_norm = IQR(auc_norm))

# SAVE THESE OUT
write_csv(output_all, "output/2022/co2/auc_norm_pooled")
write_csv(all_pooled_stats, "output/2022/co2/auc_sum_norm_pooled")

# This creates a different curve for each day and uses that to normalize
# data by day
  q <- 0
  output.1 <- mutate(output.1, low = NA, high = NA)
  # For every date (across all standards)
  for(i in 1:length(unique(output.1$date))){
    # Does each standard by date, so goes through all standards run each day
    dat1 <- filter(output.1, date == unique(output.1$date)[i])
    # Creates standard curve for that date
    linmod <- lm(log(total_auc) ~ log(sample_no), data = dat1)
    # For every instance of standard (across all standards) run in a single day
    for(j in 1:nrow(dat1)){
      q <- q+1
      new.dat <- dat1[j,]
      # Maps each standard replicate to log curve and sets highs and lows
      # according to a 95% CI (check.alpha = 0.05)
      output.1$low[q] <- predict(linmod, newdata = new.dat,
                                 interval = 'confidence',
                                 level = 1-check.alpha)[2]
      output.1$high[q] <- predict(linmod, newdata = new.dat,
                                  interval = 'confidence',
                                  level = 1-check.alpha)[3]
    }
  asdf <- summary(linmod)
  Y <- asdf$coefficients[1]
  M <- asdf$coefficients[2]
  R <- asdf$r.squared
  # Creates dataset that lists the curve generated per date
  curve[i,] <- c(unique(as.character(output.1$date))[i], Y, M, R)
  }
  # Keeps order of the dates
  preserve.order <- unique(output.1$date)

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
  inwork <- output_all %>%
    group_by(date) %>%
    summarize(n_all = n())
  # Calculates number of all standards per day
  inwork2 <- stds_only %>%
    group_by(date) %>%
    summarize(n_stds = n())
  # Checks to see if there were any non-standards in each date
  # "curve" = FALSE if there were no samples, TRUE if there were samples run
  # that day
  curve.2 <- mutate(inwork, n2 = inwork2$n_stds, "curve" = (n_all != n2))
  yes.curve <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(yes.curve) <- c("date", "Y.intercept", "Slope", "R.squared")
  # Maps each  date's standard curve if there samples run on that day
  for(i in 1:length(curve.2$date)){
    for(j in 1:length(curve$date)){
      if(curve.2$date[i] == curve$date[j] & curve.2$curve[i] == TRUE){
        yes.curve[i,] <- curve[j,]
      } else if(curve.2$curve[i] == FALSE){
        yes.curve[i,] <- c(curve.2$date[i], rep(NA,3))
      }
    }
  }
  yes.curve <- arrange(yes.curve, date)
  curve.3 <- fill(yes.curve, "date", "Y.intercept", "Slope", "R.squared",
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

  # Applies normalization to samples' AUC
  output$auc_norm <- 0
  for(i in 1:nrow(output)){
    for(j in 1:nrow(curve.3)){
      if(output$date[i] == curve.3$date[j]){
        output$auc_norm[i] <- exp((log(output$total_auc[i]) -
                                     curve.3$Y.intercept[j])/
                                    (curve.3$Slope[j]))
      }
    }
  }


  # Create summary
  summary.stats <- output %>%
    filter(is.na(auc_norm) == FALSE) %>%
    group_by(sample_no, date) %>%
    summarise(mean_norm = mean(auc_norm),
              sd_norm = sd(auc_norm),
              median_norm = median(auc_norm),
              iqr_norm = IQR(auc_norm))

  # SAVE THESE OUT
  write_csv(output, "output/2022/co2/auc_norm_date")
  write_csv(summary.stats, "output/2022/co2/auc_sum_norm_date")

