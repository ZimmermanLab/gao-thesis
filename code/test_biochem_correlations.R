# This script tests the correlation between biological and chemical elements
# in order to see the extent of microbial influence on soil NC.

# Sarah Gao
# November 2, 2022
# hellosarahgao@gmail.com

library("dplyr")
library("ggplot2")
library("ggpubr")
library("scales")

# Set plot themes
source("code/functions/set_plot_themes.R")
set_theme()

# Read in DNA summary data
dna_sum <- read_csv("data/cleaned_data/qPCR/samp_medians.csv")

# Read in CO2 summary data
co2_sum <- read_csv("data/cleaned_data/LICOR/samp_medians.csv") %>%
  rename(co2_med = median,
         co2_iqr = iqr)

# Read in NC summary data
nc_sum <- read_csv("data/cleaned_data/EA/samp_sum_all.csv")

#### DNA AND CO2 ####
dna_co2 <- dna_sum %>%
  left_join(co2_sum) %>%
  # Filter to samples with CO2 data
  filter(!is.na(co2_med))

### Bacterial DNA with CO2 ###
# Test normality
# when p > 0.05, assume data is normally distributed
shapiro.test(dna_co2$samp_med_bact)
shapiro.test(dna_co2$co2_med)

# ggqqplot draws correlation b/w sample and normal distribution
ggqqplot(dna_co2$samp_med_bact)
ggqqplot(dna_co2$co2_med)

# Plot correlation b/w bacterial DNA and CO2 respiration in
# post wet samples only
bact_dna_co2_plot <- dna_co2 %>%
  ggplot(aes(x = samp_med_bact,
             y = co2_med)) +
  geom_point(aes(color = "blue")) +
  geom_smooth(method = "lm", formula = y ~ x,
              aes(fill = "l_blue", color = "blue"), alpha = 0.25) +
  scale_fill_manual(values = c(blue = "#097CB2", l_blue = "#16B4FF",
                               green = "#195004", l_green = "#34980D")) +
  scale_color_manual(values = c(blue = "#097CB2", l_blue = "#16B4FF",
                                green = "#195004", l_green = "#34980D")) +
  labs(y = "CO2 (ppm)",
       x = "Bacterial DNA (Proportional Concentration)",
       title = "Respiration vs Bacterial DNA Quantity") +
  scale_y_continuous(labels = comma) +
  theme(legend.position = "none")
# Save out plot
ggsave(bact_dna_co2_plot,
       filename = "output/2022/correlations/bact_dna_co2_plot.png",
       width = 10, height = 8, units = "in")

# Use Kendall rank-based test for non-parametric correlation
# Tau is correlation coefficient
# p-value < 0.05 means they are significantly correlated
bact_dna_co2_stat <- cor.test(
  dna_co2$samp_med_bact, dna_co2$co2_med, method = "kendall")

### Fungal DNA with CO2 ###
# Test normality
shapiro.test(dna_co2$samp_med_fung)
ggqqplot(dna_co2$samp_med_fung)

# Plot correlation b/w fungal DNA and respiration
fung_dna_co2_plot <- dna_co2 %>%
  ggplot(aes(x = samp_med_fung,
             y = co2_med)) +
  geom_point(aes(color = "green")) +
  geom_smooth(method = "lm", formula = y ~ x,
              aes(fill = "l_green", color = "green"), alpha = 0.25) +
  scale_fill_manual(values = c(blue = "#097CB2", l_blue = "#16B4FF",
                               green = "#037D1E", l_green = "#03CC23")) +
  scale_color_manual(values = c(blue = "#097CB2", l_blue = "#16B4FF",
                                green = "#037D1E", l_green = "#03CC23")) +
  labs(y = "CO2 (ppm)",
       x = "Fungal DNA (Proportional Concentration)",
       title = "Respiration vs Fungal DNA Quantity") +
  scale_y_continuous(labels = comma) +
  theme(legend.position = "none")
ggsave(fung_dna_co2_plot,
       filename = "output/2022/correlations/fung_dna_co2_plot.png",
       width = 10, height = 8, units = "in")

# Use Kendall rank-based test for non-parametric correlation
fung_dna_co2_stat <- cor.test(
  dna_co2$samp_med_fung, dna_co2$co2_med, method = "kendall")

### CN AND CO2 ###
# Join CO2 and NC data
nc_co2 <- nc_sum %>%
  left_join(co2_sum) %>%
  filter(!is.na(co2_med))

### Correlation b/w CO2 and soil C ###
c_co2_stat <- cor.test(
  nc_co2$samp_c_med, nc_co2$co2_med, method = "kendall")

c_co2_plot <- nc_co2 %>%
  ggplot(aes(x = samp_c_med,
             y = co2_med)) +
  geom_point(aes(color = "green")) +
  geom_smooth(method = "lm", formula = y ~ x,
              aes(fill = "l_green", color = "green"), alpha = 0.25) +
  scale_fill_manual(values = c(blue = "#097CB2", l_blue = "#16B4FF",
                               green = "#037D1E", l_green = "#03CC23")) +
  scale_color_manual(values = c(blue = "#097CB2", l_blue = "#16B4FF",
                                green = "#037D1E", l_green = "#03CC23")) +
  labs(y = "CO2 (ppm)",
       x = "% Carbon",
       title = "Respiration vs Total Soil Carbon") +
  scale_y_continuous(labels = comma) +
  theme(legend.position = "none")
ggsave(c_co2_plot,
       filename = "output/2022/correlations/c_co2_plot.png",
       width = 10, height = 8, units = "in")

### Correlation b/w CO2 and soil N ###
n_co2_stat <- cor.test(
  nc_co2$samp_n_med, nc_co2$co2_med, method = "kendall")

n_co2_plot <- nc_co2 %>%
  ggplot(aes(x = samp_n_med,
             y = co2_med)) +
  geom_point(aes(color = "blue")) +
  geom_smooth(method = "lm", formula = y ~ x,
              aes(fill = "l_blue", color = "blue"), alpha = 0.25) +
  scale_fill_manual(values = c(blue = "#097CB2", l_blue = "#16B4FF",
                               green = "#037D1E", l_green = "#03CC23")) +
  scale_color_manual(values = c(blue = "#097CB2", l_blue = "#16B4FF",
                                green = "#037D1E", l_green = "#03CC23")) +
  labs(y = "CO2 (ppm)",
       x = "% Nitrogen",
       title = "Respiration vs Total Soil Nitrogen") +
  scale_y_continuous(labels = comma) +
  theme(legend.position = "none")
ggsave(n_co2_plot,
       filename = "output/2022/correlations/n_co2_plot.png",
       width = 10, height = 8, units = "in")

### Correlation b/w CO2 and soil C:N ###
cn_co2_stat <- cor.test(
  nc_co2$samp_cn_med, nc_co2$co2_med, method = "kendall")

cn_co2_plot <- nc_co2 %>%
  ggplot(aes(x = samp_cn_med,
             y = co2_med)) +
  geom_point(aes(color = "blue")) +
  geom_smooth(method = "lm", formula = y ~ x,
              aes(fill = "l_blue", color = "blue"), alpha = 0.25) +
  scale_fill_manual(values = c(blue = "#097CB2", l_blue = "#16B4FF",
                               green = "#037D1E", l_green = "#03CC23")) +
  scale_color_manual(values = c(blue = "#097CB2", l_blue = "#16B4FF",
                                green = "#037D1E", l_green = "#03CC23")) +
  labs(y = "CO2 (ppm)",
       x = "C:N Ratio",
       title = "Respiration vs Soil Total C:N") +
  scale_y_continuous(labels = comma) +
  theme(legend.position = "none")
ggsave(cn_co2_plot,
       filename = "output/2022/correlations/cn_co2_plot.png",
       width = 10, height = 8, units = "in")


### NC AND DNA PRE/POST WET ###
# Join NC and DNA data and select only pre/post wet
dna_nc <- nc_sum %>%
  left_join(dna_sum) %>%
  filter(pre_post_wet == "pre" |
           pre_post_wet == "post")

### Correlation between bacteria and N ###
bact_n_stat <- cor.test(
  dna_nc$samp_n_med, dna_nc$samp_med_bact, method = "kendall")

bact_n_plot <- dna_nc %>%
  ggplot(aes(x = samp_n_med,
             y = samp_med_bact)) +
  geom_point(aes(color = "blue")) +
  geom_smooth(method = "lm", formula = y ~ x,
              aes(fill = "l_blue", color = "blue"), alpha = 0.25) +
  scale_fill_manual(values = c(blue = "#097CB2", l_blue = "#16B4FF",
                               green = "#037D1E", l_green = "#03CC23")) +
  scale_color_manual(values = c(blue = "#097CB2", l_blue = "#16B4FF",
                                green = "#037D1E", l_green = "#03CC23")) +
  labs(y = "Bacterial DNA (Proportional Concentration)",
       x = "% Nitrogen",
       title = "Bacteria DNA Quantity vs Total Soil Nitrogen") +
  scale_y_continuous(labels = comma) +
  theme(legend.position = "none")
ggsave(bact_n_plot,
       filename = "output/2022/correlations/bact_n_plot.png",
       width = 10, height = 8, units = "in")

### Correlation between bacteria and C ###
bact_c_stat <- cor.test(
  dna_nc$samp_c_med, dna_nc$samp_med_bact, method = "kendall")

bact_c_plot <- dna_nc %>%
  ggplot(aes(x = samp_c_med,
             y = samp_med_bact)) +
  geom_point(aes(color = "blue")) +
  geom_smooth(method = "lm", formula = y ~ x,
              aes(fill = "l_blue", color = "blue"), alpha = 0.25) +
  scale_fill_manual(values = c(blue = "#097CB2", l_blue = "#16B4FF",
                               green = "#037D1E", l_green = "#03CC23")) +
  scale_color_manual(values = c(blue = "#097CB2", l_blue = "#16B4FF",
                                green = "#037D1E", l_green = "#03CC23")) +
  labs(y = "Bacterial DNA (Proportional Concentration)",
       x = "% Carbon",
       title = "Bacteria DNA Quantity vs Total Soil Carbon") +
  scale_y_continuous(labels = comma) +
  theme(legend.position = "none")
ggsave(bact_c_plot,
       filename = "output/2022/correlations/bact_c_plot.png",
       width = 10, height = 8, units = "in")

### Correlation between fungi and N ###
fung_n_stat <- cor.test(
  dna_nc$samp_n_med, dna_nc$samp_med_fung, method = "kendall")

fung_n_plot <- dna_nc %>%
  ggplot(aes(x = samp_n_med,
             y = samp_med_fung)) +
  geom_point(aes(color = "green")) +
  geom_smooth(method = "lm", formula = y ~ x,
              aes(fill = "l_green", color = "green"), alpha = 0.25) +
  scale_fill_manual(values = c(blue = "#097CB2", l_blue = "#16B4FF",
                               green = "#037D1E", l_green = "#03CC23")) +
  scale_color_manual(values = c(blue = "#097CB2", l_blue = "#16B4FF",
                                green = "#037D1E", l_green = "#03CC23")) +
  labs(y = "Fungal DNA (Proportional Concentration)",
       x = "% Nitrogen",
       title = "Fungal DNA Quantity vs Total Soil Nitrogen") +
  scale_y_continuous(labels = comma) +
  theme(legend.position = "none")
ggsave(fung_n_plot,
       filename = "output/2022/correlations/fung_n_plot.png",
       width = 10, height = 8, units = "in")

### Correlation between fungi and C ###
fung_c_stat <- cor.test(
  dna_nc$samp_c_med, dna_nc$samp_med_fung, method = "kendall")

fung_c_plot <- dna_nc %>%
  ggplot(aes(x = samp_c_med,
             y = samp_med_fung)) +
  geom_point(aes(color = "green")) +
  geom_smooth(method = "lm", formula = y ~ x,
              aes(fill = "l_green", color = "green"), alpha = 0.25) +
  scale_fill_manual(values = c(blue = "#097CB2", l_blue = "#16B4FF",
                               green = "#037D1E", l_green = "#03CC23")) +
  scale_color_manual(values = c(blue = "#097CB2", l_blue = "#16B4FF",
                                green = "#037D1E", l_green = "#03CC23")) +
  labs(y = "Fungal DNA (Proportional Concentration)",
       x = "% Carbon",
       title = "Fungal DNA Quantity vs Total Soil Carbon") +
  scale_y_continuous(labels = comma) +
  theme(legend.position = "none")
ggsave(fung_c_plot,
       filename = "output/2022/correlations/fung_c_plot.png",
       width = 10, height = 8, units = "in")

### Correlation between bacteria and c:N ###
bact_cn_stat <- cor.test(
  dna_nc$samp_cn_med, dna_nc$samp_med_bact, method = "kendall")

bact_cn_plot <- dna_nc %>%
  ggplot(aes(x = samp_cn_med,
             y = samp_med_bact)) +
  geom_point(aes(color = "blue")) +
  geom_smooth(method = "lm", formula = y ~ x,
              aes(fill = "l_blue", color = "blue"), alpha = 0.25) +
  scale_fill_manual(values = c(blue = "#097CB2", l_blue = "#16B4FF",
                               green = "#037D1E", l_green = "#03CC23")) +
  scale_color_manual(values = c(blue = "#097CB2", l_blue = "#16B4FF",
                                green = "#037D1E", l_green = "#03CC23")) +
  labs(y = "Bacterial DNA (Proportional Concentration)",
       x = "C:N Ratio",
       title = "Bacteria DNA Quantity vs Soil Total C:N") +
  scale_y_continuous(labels = comma) +
  theme(legend.position = "none")
ggsave(bact_cn_plot,
       filename = "output/2022/correlations/bact_cn_plot.png",
       width = 10, height = 8, units = "in")

### Correlation between fungi and C:N ###
fung_cn_stat <- cor.test(
  dna_nc$samp_cn_med, dna_nc$samp_med_fung, method = "kendall")

fung_cn_plot <- dna_nc %>%
  ggplot(aes(x = samp_cn_med,
             y = samp_med_fung)) +
  geom_point(aes(color = "green")) +
  geom_smooth(method = "lm", formula = y ~ x,
              aes(fill = "l_green", color = "green"), alpha = 0.25) +
  scale_fill_manual(values = c(blue = "#097CB2", l_blue = "#16B4FF",
                               green = "#037D1E", l_green = "#03CC23")) +
  scale_color_manual(values = c(blue = "#097CB2", l_blue = "#16B4FF",
                                green = "#037D1E", l_green = "#03CC23")) +
  labs(y = "Fungal DNA (Proportional Concentration)",
       x = "C:N Ratio",
       title = "Fungal DNA Quantity vs Soil Total C:N") +
  scale_y_continuous(labels = comma) +
  theme(legend.position = "none")
ggsave(fung_cn_plot,
       filename = "output/2022/correlations/fung_cn_plot.png",
       width = 10, height = 8, units = "in")

### Correlation between fungi and bacteria ###
fung_cn_plot <- dna_nc %>%
  ggplot(aes(x = samp_cn_med,
             y = samp_med_fung)) +
  geom_point(aes(color = "green")) +
  geom_smooth(method = "lm", formula = y ~ x,
              aes(fill = "l_green", color = "green"), alpha = 0.25) +
  scale_fill_manual(values = c(blue = "#097CB2", l_blue = "#16B4FF",
                               green = "#037D1E", l_green = "#03CC23")) +
  scale_color_manual(values = c(blue = "#097CB2", l_blue = "#16B4FF",
                                green = "#037D1E", l_green = "#03CC23")) +
  labs(y = "Fungal DNA (Proportional Concentration)",
       x = "C:N Ratio",
       title = "Fungal DNA Quantity vs Soil Total C:N") +
  scale_y_continuous(labels = comma) +
  theme(legend.position = "none")
ggsave(fung_cn_plot,
       filename = "output/2022/correlations/fung_cn_plot.png",
       width = 10, height = 8, units = "in")

ggscatter(dna_sum, x = "samp_med_bact", y = "samp_med_fung",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "kendall")

### Fungi and bacteria in no cc vs w cc ###
dna_sum_nocc <- dna_sum %>%
  filter(cc_treatment == "no_cc")
ggscatter(dna_sum_nocc, x = "samp_med_bact", y = "samp_med_fung",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "kendall")

dna_sum_wcc <- dna_sum %>%
  filter(cc_treatment == "w_cc")
ggscatter(dna_sum_wcc, x = "samp_med_bact", y = "samp_med_fung",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "kendall")


### DNA ratio to C:N ratio ###
dna_ratio <- dna_sum %>%
  mutate(samp_ratio = samp_med_fung / samp_med_bact)

dna_cn_ratio <- dna_ratio %>%
  left_join(nc_sum)

ggscatter(dna_cn_ratio, x = "samp_ratio", y = "samp_cn_med",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "kendall")
