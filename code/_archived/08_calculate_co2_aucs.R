# This script calculates AUCs for the CO2 LI-COR data and saves it out to a csv file

# Sarah Gao
# November 19, 2022
# hellosarahgao@gmail.com


# Source function to calculate AUCs
source("code/functions/co2_functions/find_peaks.R")

# Create list of file paths of CO2 data files
files_list <- list.files(path = "data/cleaned_data/LICOR/", recursive = FALSE,
                         pattern = "*.txt")
files_list <- subset(files_list, str_detect(files_list, "extract") == FALSE)

auc_summary_all <- data.frame()

for(f in 1:length(files_list)) {
  auc_summary <- find_peaks(files_list[f])
  auc_summary_all <- rbind(auc_summary_all, auc_summary)
}
# Save this out because it takes a long time to generate
write_csv(auc_summary_all, "output/2022/co2/auc_summary_all.txt")
