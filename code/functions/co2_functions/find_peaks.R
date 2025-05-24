# This function takes in a .txt file with LICOR CO2 data and finds the area
# under the curve.

# This is modified from Jacob Anderson's peak.gas package:
# https://github.com/andersonjake1988/peak.gas

library("lubridate")

find_peaks <- function(file_path, cut.off = 2) {

  # Creates a Peaks function that identifies CO2 readings that are higher
  # than the manually set cut.off threshold (baseline)
  Peaks <- function(x){
    output <- vector()
    for(i in 1:length(x)){
      ifelse(x[i] >= cut.off, output[i] <- x[i], output[i] <- NA)
    }
    output
  }

  # Reads in file
  raw_file <- read.csv(paste0("data/cleaned_data/LICOR/", file_path),
                       header = TRUE, sep = "\t", fill = TRUE,
                strip.white = TRUE, check.names = FALSE)

  data.1 <- raw_file %>%
    # Selects only Date, Time, and CO2
    select(1:3) %>%
    rename("Date" = 1,
           "Time" = 2,
           "CO2" = 3) %>%
    # Make sure to filter out any extra headers embedded in the data
    filter(Date != "System_Date_(Y-M-D)") %>%
    filter(!(str_detect(Date, "at"))) %>%
    mutate(Time = paste(Date, Time)) %>%
    select(-Date)
  # Convert times into doubles
  data.1$Time <- lubridate::as_datetime(data.1$Time)
  # Ensure all columns are double
  data.1$CO2 <- as.double(data.1$CO2)

  # Identify rows that will be used for peak calculation
  test.2 <- data.1 %>%
    mutate("Peaks" = Peaks(CO2)) %>%
    mutate("Value" = !is.na(Peaks), "Replicate" = NA)

  # Add time column
  test.2 <- arrange(test.2, Time)
  r <- 0
  for(i in 1:(length(test.2$Value)-1)){
    if(test.2$Value[i] == TRUE & test.2$Value[i+1] == TRUE){
      test.2$Replicate[i] <- r
    } else if(test.2$Value[i] == FALSE & test.2$Value[i+1] == TRUE){
      r <- r + 1
    } else if(test.2$Value[i] == FALSE & test.2$Value[i+1] == FALSE){
      test.2$Replicate[i] <- NA
    } else {
      test.2$Replicate[i] <- r
    }
  }

  # Sets baseline to cut.off value (default = 2)
  for(i in 2:(length(test.2$Value)-1)){
    if(test.2$Value[i] == FALSE & test.2$Value[i+1] == TRUE){
      test.2$Peaks[i] <- cut.off
      test.2$Replicate[i] <- test.2$Replicate[i+1]
    } else if(test.2$Value[i] == FALSE & test.2$Value[i-1] == TRUE){
      test.2$Peaks[i] <- cut.off
      test.2$Replicate[i] <- test.2$Replicate[i-1]
    } else {
      next
    }
  }
  test.3 <- na.omit(test.2)
  test.3 <- mutate(test.3, "Area" = 0)
  test.3$diff <- c(0, diff(test.3$Peaks))
  peaks <- test.3$Peaks-cut.off
  diff <- c(0, diff(peaks))
  for(i in 1:(length(test.3$Peaks)-1)){
    if(test.3$Replicate[i] == test.3$Replicate[i+1]){
      time <- as.numeric(difftime(test.3$Time[i+1], test.3$Time[i]))
      # Finds area at the beginning of the curve
      if(test.3$Value[i] == FALSE & test.3$Value[i+1] == TRUE){
        test.3$Area[i] <- (time * diff[i+1])/2
        # Finds area between curves
      } else if(test.3$Value[i] == FALSE & test.3$Value[i+1] == FALSE){
        test.3$Area[i] <- (time * diff[i])/2
      } else if(test.3$Value[i] == TRUE){
        test.3$Area[i] <- (peaks[i] * time) + ((time * diff[i+1])/2)
      }
    } else{
      next
    }
  }
  test.4 <- test.3 %>%
    group_by(Replicate)%>%
    summarize("AUC" = sum(Area), "Peak" = max(Peaks),
              "Time_Peak_Start" = min(Time), "Time_Peak_End" = max(Time)) %>%
    arrange(Time_Peak_Start)

  # Create a summary that includes the sample name + replicate number, date
  # of injection, and total AUC.
  auc_summary <- data.frame(sample =
                              str_sub(basename(file_path), start = 10, end =-13),
                            date = str_sub(data.1$Time[1],
                                           end = 10),
                            total_auc = sum(test.4$AUC))
  return(auc_summary)
}

