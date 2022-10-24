# This script cleans data from qPCR assays

# Sarah Gao
# October 24, 2022
# hellosarahgao@gmail.com

library("tidyr")
library("dplyr")
library("readr")
library("stringi")
library("stringr")
library("ggplot2")

clean_qpcr_data <- function(input_files, type) {
  # Read in files, depending on whether fungal or bacterial
  ifelse(type = "bacterial")
  files_fungal <- dir_ls(path = "data/raw_data/qPCR/",
                         recurse = 1,
                         regex = "fungal -  Quantification Cq Results.csv")


}
