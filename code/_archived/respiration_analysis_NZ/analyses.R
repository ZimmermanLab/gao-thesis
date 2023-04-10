# script to parse respiration data
# Naupaka Zimmerman nzimmerman@usfca.edu February 4, 2022

library("tidyverse")
library("lubridate")
library("janitor")
library("baseline")
library("readr")
library("dplyr")
library("baseline")
library("ggplot2")

# load data
read_delim("data/20220111_tests/20220111_04_prewet.txt",
           skip = 1) %>%
  janitor::clean_names() %>%
  mutate(diff = c(0, diff(.$co2_mmol_mol_1, ))) -> concentration_data

summary(concentration_data$diff)

baseline(as.matrix(concentration_data)[, 3, drop = FALSE], wm = 0.1, ws = 0.1,
         method = 'rollingBall')

# check flow rate over time
ggplot(concentration_data,
       aes(x = lubridate::hms(system_time_h_m_s),
           y = flow_rate_l_min_1)) +
  geom_point() +
  scale_x_time()

# check pressure over time
ggplot(concentration_data,
       aes(x = lubridate::hms(system_time_h_m_s),
           y = cell_pressure_k_pa)) +
  geom_point() +
  scale_x_time()

# check pressure vs flow rate over time
ggplot(concentration_data,
       aes(x = cell_pressure_k_pa,
           y = flow_rate_l_min_1)) +
  geom_point() +
  scale_x_time()

summary(lm(data = concentration_data,
         flow_rate_l_min_1 ~ cell_pressure_k_pa))

# check temperature over time
ggplot(concentration_data,
       aes(x = lubridate::hms(system_time_h_m_s),
           y = cell_temperature_c)) +
  geom_point() +
  scale_x_time()

# check flow rate over time
ggplot(concentration_data,
       aes(x = lubridate::hms(system_time_h_m_s),
           y = co2_mmol_mol_1 * flow_rate_l_min_1)) +
  geom_line() +
  scale_x_time()

# check concentration over time
# NOTE 1 mmol/mol = 1000 ppm
ggplot(concentration_data,
       aes(x = lubridate::hms(system_time_h_m_s),
           y = co2_mmol_mol_1)) +
  geom_line() +
  scale_x_time()


all_data <- purrr::map_dfr(.x = list.files(path = "data/20220111_tests", pattern = ".txt", full.names = TRUE),
           .f = ~ read_delim(.x, skip = 1))

all_data_cleaned_names <- purrr::map(all_data, janitor::clean_names)

purrr::map(all_data_cleaned_names,
           ~plot(.x$co2_mmol_mol_1, type = "l"))

max_co2 <- purrr::map_dbl(all_data_cleaned_names,
           ~max(.x$co2_mmol_mol_1))

filenames <- list.files(path = "data/20220111_tests", pattern = ".txt")

as.data.frame(cbind(filenames, max_co2)) %>%
  separate(filenames, sep = "_", into = c("date", "number", "timepoint")) %>%
  mutate(timepoint = gsub(timepoint, pattern = ".txt", replacement = "")) %>%
  mutate(timepoint = gsub(timepoint, pattern = "hr", replacement = "")) %>%
  mutate(timepoint = recode(timepoint, prewet = "0")) %>%
  mutate(max_co2 = as.numeric(max_co2)) %>%
  mutate(timepoint = as.numeric(timepoint)) %>%
  ggplot(aes(x = timepoint, group = timepoint)) +
  geom_boxplot(aes(y = max_co2), alpha = 0.2) +
  geom_jitter(aes(y = max_co2, color = number),
              size = 5, width = 0.2, alpha = 0.4) +
  labs(title = "Max CO2 in mmol/mol (1 mmol/mol = 1000 ppm)",
       subtitle = "Note: not yet baseline corrected")
