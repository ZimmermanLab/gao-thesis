# This script analyses the relationship between DNA and RNA purity,
# concentration, and amplification

# Sarah Gao
# April 29, 2022
# hellosarahgao@gmail.com

library("readr")
library("tidyr")
library("dplyr")
library("ggplot2")

# Read in Nanodrop data
raw_nanodrop <- read_csv(
  "data/raw_data/Nanodrop/20220412/20220412_dsDNA_nanodrop.csv") %>%
  select("sample_id", "concentration_ng/uL", "A260/A280", "A260/A230")

# Read in amplification data


