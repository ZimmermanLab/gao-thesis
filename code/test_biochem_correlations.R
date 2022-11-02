# This script tests the correlation between biological and chemical elements
# in order to see the extent of microbial influence on soil NC.

# Sarah Gao
# November 2, 2022
# hellosarahgao@gmail.com

library("dplyr")
library("ggplot2")
library("ggpubr")

# Read in DNA summary data
dna_sum <- read_csv("data/cleaned_data/qPCR/samp_medians.csv")

# Read in CO2 summary data
co2_sum <- read_csv("data/cleaned_data/LICOR/samp_medians.csv")

#### DNA AND CO2 ####
dna_co2 <- dna_sum %>%
  left_join(co2_sum) %>%
  select(-c(date, day_elapsed)) %>%
  rename(co2_med = median,
         co2_iqr = iqr) %>%
  # Filter to samples with CO2 data
  filter(!is.na(co2_med))

dna_co2 %>%
  ggscatter(., x = "samp_med_bact",
             y = "co2_med",
         add = "reg.line", conf.int = TRUE,
         cor.coef = TRUE, cor.method = "pearson")
# Test normality
# when p > 0.05, assume data is normally distributed
shapiro.test(dna_co2$samp_med_bact)
# Draws correlation b/w sample and normal distribution
ggqqplot(dna_co2$samp_med_bact)

shapiro.test(dna_co2$samp_med_fung)
ggqqplot(dna_co2$samp_med_fung)

shapiro.test(dna_co2$co2_med)
ggqqplot(dna_co2$co2_med)

# Test fungal DNA with CO2
# Use Kendall rank-based test for non-parametric correlation
dna_co2 %>%
  ggscatter(., x = "samp_med_fung",
            y = "co2_med",
            add = "reg.line", conf.int = TRUE,
            cor.coef = TRUE, cor.method = "kendall")

# Tau is correlation coefficient
# p-value < 0.05 means they are significantly correlated
cor.test(dna_co2$samp_med_fung, dna_co2$co2_med, method = "kendall")

# Test bacterial DNA with CO2
dna_co2 %>%
  ggplot(aes(x = samp_med_bact,
             y = co2_med)) +
  geom_point() +
  geom_smooth(method='lm', formula= y~x)

dna_co2 %>%
  ggscatter(., x = "samp_med_bact",
            y = "co2_med",
            add = "reg.line", conf.int = TRUE,
            cor.coef = TRUE, cor.method = "kendall")
cor.test(dna_co2$samp_med_bact, dna_co2$co2_med, method = "kendall")


