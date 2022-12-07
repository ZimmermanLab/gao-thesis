# This script compiles all the datasets into one beefy dataframe

# Sarah Gao
# December 7, 2022
# hellosarahgao@gmail.com

library("dplyr")

# Read in DNA data
dna <- read_csv("data/cleaned_data/qPCR/samp_medians.csv")

# Read in CO2 data
co2 <- read_csv("data/cleaned_data/LICOR/samp_medians.csv")

# Read in total CN data
total_cn <- read_csv("data/cleaned_data/EA/samp_sum_all.csv")
