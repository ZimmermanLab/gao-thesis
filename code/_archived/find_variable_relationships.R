# This script creates GLMs based on a fully compiled dataset of all
# measurements to find significant causal relationships

# Sarah Gao
# December 7, 2022
# hellosarahgao@gmail.com

library("dplyr")
library("glmmTMB")

# Bring in fully compiled dataframe
source("code/functions/compile_datasets.R")
all_data <- compile_datasets()

# Create GLMs and test for significant causality between cc_treatment and
# drought_type and the following variables:

# DNA predicts CO2
all_data %>%
  filter(cc_treatment == "w_cc") %>%
  glm(data = ., peak_co2_med ~ samp_med_bact * samp_med_fung,
      family = "Gamma") %>%
  summary()

# Total CN predicts CO2
all_data %>%
  glm(data = ., peak_co2_med ~ samp_n_med * samp_c_med * cc_treatment) %>%
  summary()
# CO2 predict Total CN
all_data %>%
  glm(data = ., samp_c_med ~ peak_co2_med) %>%
  summary()

# DNA predicts Total N
all_data %>%
  glm(data = ., samp_n_med ~ samp_med_bact * samp_med_fung * cc_treatment * drought_type) %>%
  summary()
all_data  %>%
  glm(data = ., samp_c_med ~ samp_med_bact * samp_med_fung * cc_treatment * drought_type) %>%
  summary()
all_data %>%
  filter(cc_treatment == "no_cc") %>%
  glm(data = ., samp_med_bact ~ samp_n_med * samp_c_med,
      family = "Gamma") %>%
  summary()

# DNA affected by drought type, cc treatment, time
all_data %>%
  glm(data = ., samp_med_bact ~ cc_treatment * drought_type * time) %>%
  summary()
all_data %>%
  glm(data = ., samp_med_fung ~ cc_treatment * drought_type * time) %>%
  summary()

# Inorganic N affected by drought type, cc treatment, time
all_data %>%
  glm(data = ., samp_n_med ~ cc_treatment * drought_type * time) %>%
  summary()
all_data %>%
  glm(data = ., samp_med_fung ~ cc_treatment * drought_type * time) %>%
  summary()

all_data %>%
  glm(data = ., peak_co2_med ~ samp_n_med * samp_c_med) %>%
  summary()


