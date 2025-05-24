# This script tests the correlation between biological and chemical elements
# in order to see the extent of microbial influence on soil NC.

# Sarah Gao
# November 2, 2022
# hellosarahgao@gmail.com

library("dplyr")
library("ggplot2")
library("ggpubr")
library("scales")
library("patchwork")

# Set plot themes
source("code/functions/set_plot_themes.R")
set_theme("doc")

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

# Filter by no_cc and w_cc
dna_co2_nocc <- dna_co2 %>%
  filter(cc_treatment == "no_cc")
dna_co2_wcc <- dna_co2 %>%
  filter(cc_treatment == "w_cc")

# Use Kendall rank-based test for non-parametric correlation
# Tau is correlation coefficient
# p-value < 0.05 means they are significantly correlated
bact_co2_nocc_stat <- cor.test(
  dna_co2_nocc$samp_med_bact, dna_co2_nocc$co2_med, method = "kendall")
bact_co2_wcc_stat <- cor.test(
  dna_co2_wcc$samp_med_bact, dna_co2_wcc$co2_med, method = "kendall")

### Fungal DNA with CO2 ###
# Test normality
shapiro.test(dna_co2$samp_med_fung)
ggqqplot(dna_co2$samp_med_fung)

# Use Kendall rank-based test for non-parametric correlation
fung_co2_nocc_stat <- cor.test(
  dna_co2_nocc$samp_med_fung, dna_co2_nocc$co2_med, method = "kendall")
fung_co2_wcc_stat <- cor.test(
  dna_co2_wcc$samp_med_fung, dna_co2_wcc$co2_med, method = "kendall")

# Plot correlation b/w DNA and CO2 respiration in
# post wet samples w cc only
facet_type_labels <- as_labeller(c("samp_med_bact" = "Bacteria",
                                   "samp_med_fung" = "Fungi"))
# Create Kendall annotation
kendall_annot_dna <- data.frame(type = "samp_med_fung",
                           label = "Kendall Rank Correlation")

# Side by side of bacterial and fungal DNA
dna_co2_wcc_plot <- dna_co2_long %>%
  filter(cc_treatment == "w_cc") %>%
  mutate(dna_quant_e6 = dna_quant * 1e6) %>%
  ggplot(aes(x = dna_quant_e6,
             y = co2_med)) +
  geom_point(aes(fill = factor(type),
                 color = factor(type))) +
  geom_smooth(method = "lm", formula = y ~ x,
              aes(fill = type, color = factor(type)), alpha = 0.25) +
  facet_wrap(~ type,
             scales = "free",
             labeller = facet_type_labels,
             dir = "h") +
  scale_fill_manual(values = c("#097CB2", "#03CC23")) +
  scale_color_manual(values = c("#16B4FF", "#39B708")) +
  labs(y = "CO2 (ppm)", x = "DNA (Proportional Concentration)",
       title =
         "CO2 Concentration vs DNA Quantities\nin Rewet Soils With Cover Crop") +
  scale_y_continuous(labels = comma) +
  theme(legend.position = "none",
        panel.spacing = unit(1, "lines")) +
  # Add correlation stats
  stat_cor(method = "kendall", label.x.npc = "left", label.y.npc = "top",
           p.accuracy = 0.001,
           family = "Helvetica",
           size = 5,
           lineheight = 0.9) +
  # Add test annotation
  geom_text(x = 3.3, y = 25000, data = kendall_annot_dna,
            aes(label = label,
                family = "Helvetica",
                size = 16,
                lineheight = 0.9),
            show.legend = F)
ggsave(dna_co2_wcc_plot,
       filename = "output/2022/correlations/micro_co2_wcc_plot.png",
       width = 14, height = 8, units = "in")

#### CN AND CO2 ####
# Join CO2 and NC data
nc_co2 <- nc_sum %>%
  left_join(co2_sum) %>%
  filter(!is.na(co2_med))
#Separate by cc treatment
nc_co2_nocc <- nc_co2 %>%
  filter(cc_treatment == "no_cc")
nc_co2_wcc <- nc_co2 %>%
  filter(cc_treatment == "w_cc")

# Correlation b/w CO2 and soil CN in no cc
c_co2_nocc_stat <- cor.test(
  nc_co2_nocc$samp_c_med, nc_co2_nocc$co2_med, method = "kendall")
n_co2_nocc_stat <- cor.test(
  nc_co2_nocc$samp_n_med, nc_co2_nocc$co2_med, method = "kendall")
# W cc
c_co2_wcc_stat <- cor.test(
  nc_co2_wcc$samp_c_med, nc_co2_wcc$co2_med, method = "kendall")
n_co2_wcc_stat <- cor.test(
  nc_co2_wcc$samp_n_med, nc_co2_wcc$co2_med, method = "kendall")

# Correlation b/w CO2 and soil CN all
c_co2_stat <- cor.test(
  nc_co2$samp_c_med, nc_co2$co2_med, method = "kendall")
n_co2_stat <- cor.test(
  nc_co2$samp_n_med, nc_co2$co2_med, method = "kendall")

# Create Kendall annotation
kendall_annot_cn <- data.frame(type = "samp_n_med",
                                label = "Kendall Rank Correlation")
# Side by side plot of CO2 and %N and %C
nc_co2_wcc_plot <- nc_co2_wcc %>%
  pivot_longer(cols = c(samp_n_med, samp_c_med),
               names_to = "type", values_to = "med") %>%
  ggplot(aes(x = med,
             y = co2_med)) +
  geom_point(aes(
    fill = factor(type),
    color = factor(type))) +
  geom_smooth(method = "lm", formula = y ~ x,
              aes(fill = type, color = factor(type)), alpha = 0.25) +
  facet_wrap(~ type,
             scales = "free",
             labeller = as_labeller(c("samp_n_med" = "Nitrogen",
                                 "samp_c_med" = "Carbon")),
             dir = "h") +
  scale_fill_manual(values = c("#03CC23", "#097CB2")) +
  scale_color_manual(values = c( "#39B708", "#16B4FF")) +
  labs(x = "Element %", y = "CO2 (ppm)",
       title =
         "CO2 Concentration vs Total Soil CN\nin Rewet Soils With Cover Crop") +
  scale_y_continuous(labels = comma,
                     limits = c(0, 220000)) +
  theme(legend.position = "none",
        panel.spacing = unit(1, "lines")) +
  # Add correlation stats
  stat_cor(method = "kendall", label.x.npc = "left", label.y.npc = "top",
           p.accuracy = 0.001,
           family = "Helvetica",
           size = 5,
           lineheight = 0.9) +
  # Add test annotation
  geom_text(x = 0.415, y = 8000, data = kendall_annot_cn,
            aes(label = label,
                family = "Helvetica",
                size = 16,
                lineheight = 0.9),
            show.legend = F)

ggsave(nc_co2_wcc_plot,
       filename = "output/2022/correlations/nc_co2_wcc_plot.png",
       width = 14, height = 8, units = "in")

### Correlation b/w CO2 and soil C:N ###
cn_co2_nocc_stat <- cor.test(
  nc_co2_nocc$samp_cn_med, nc_co2_nocc$co2_med, method = "kendall")
cn_co2_wcc_stat <- cor.test(
  nc_co2_wcc$samp_cn_med, nc_co2_wcc$co2_med, method = "kendall")

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


#### NC AND DNA PRE/POST WET ####
# Join NC and DNA data and select only pre/post wet
# No cc
dna_nc_nocc <- nc_sum %>%
  left_join(dna_sum) %>%
  filter(cc_treatment == "no_cc")

dna_nc_nocc_prepost <- dna_nc_nocc %>%
  filter(pre_post_wet == "pre" |
           pre_post_wet == "post")
dna_nc_nocc_pre <- dna_nc_nocc %>%
  filter(pre_post_wet == "pre")
dna_nc_nocc_post <- dna_nc_nocc %>%
  filter(pre_post_wet == "post")
dna_nc_nocc_dry <- dna_nc_nocc %>%
  filter(pre_post_wet == "pre" |
           pre_post_wet == "all_dry")

# W cc
dna_nc_wcc <- nc_sum %>%
  left_join(dna_sum) %>%
  filter(cc_treatment == "w_cc")

dna_nc_wcc_prepost <- dna_nc_wcc %>%
  filter(pre_post_wet == "pre" |
           pre_post_wet == "post")
dna_nc_wcc_pre <- dna_nc_wcc %>%
  filter(pre_post_wet == "pre")
dna_nc_wcc_post <- dna_nc_wcc %>%
  filter(pre_post_wet == "post")
dna_nc_wcc_dry <- dna_nc_wcc %>%
  filter(pre_post_wet == "pre" |
           pre_post_wet == "all_dry")

ggscatter(dna_nc_nocc, y = "samp_med_fung", x = "samp_n_med",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "kendall")



# Correlation between bacteria and N in pre vs post
# No cc
bact_n_nocc_stat_pre <- cor.test(
  dna_nc_nocc_pre$samp_n_med, dna_nc_nocc_pre$samp_med_bact, method = "kendall")
bact_n_noccstat_post <- cor.test(
  dna_nc_nocc_post$samp_n_med, dna_nc_nocc_post$samp_med_bact,
  method = "kendall")
# W cc
bact_n_wcc_stat_pre <- cor.test(
  dna_nc_wcc_pre$samp_n_med, dna_nc_wcc_pre$samp_med_bact, method = "kendall")
bact_n_wccstat_post <- cor.test(
  dna_nc_wcc_post$samp_n_med, dna_nc_wcc_post$samp_med_bact,
  method = "kendall")

# Correlation between fungi and N in pre vs post
# No cc
fung_n_nocc_stat_pre <- cor.test(
  dna_nc_nocc_pre$samp_n_med, dna_nc_nocc_pre$samp_med_fung, method = "kendall")
fung_n_nocc_stat_post <- cor.test(
  dna_nc_nocc_post$samp_n_med, dna_nc_nocc_post$samp_med_fung,
  method = "kendall")
# W cc
fung_n_wcc_stat_pre <- cor.test(
  dna_nc_wcc_pre$samp_n_med, dna_nc_wcc_pre$samp_med_fung, method = "kendall")
fung_n_wcc_stat_post <- cor.test(
  dna_nc_wcc_post$samp_n_med, dna_nc_wcc_post$samp_med_fung,
  method = "kendall")

# 4 x 4 Facet plot of bacteria and fungi vs %N in pre vs post with no cc
facet_prepost_labels <- c("b_post" = "Post-Wet", "a_pre" = "Pre-Wet")
micro_n_nocc_plot <- dna_nc_nocc_prepost %>%
  mutate(pre_post_wet =
           # Terrible hack to reorder facets
           case_when(str_detect(pre_post_wet, "pre") ~ "a_pre",
                     str_detect(pre_post_wet, "post") ~ "b_post")) %>%
  pivot_longer(cols = c(samp_med_bact, samp_med_fung),
               names_to = "type", values_to = "med") %>%
  mutate(med = med * 1e6) %>%
  ggplot(aes(x = samp_n_med,
             y = med)) +
  geom_point(aes(fill = factor(type),
                 color = factor(type))) +
  geom_smooth(method = "lm", formula = y ~ x,
              aes(fill = type, color = factor(type)), alpha = 0.25) +
  facet_grid(type ~ pre_post_wet,
             scales = "free",
             labeller = (labeller(type = facet_type_labels,
                                  pre_post_wet = facet_prepost_labels))) +
  scale_fill_manual(values = c("#097CB2", "#03CC23")) +
  scale_color_manual(values = c("#16B4FF", "#39B708")) +
  labs(y = element_blank(),
       x = "% Nitrogen",
       title = "Nitrogen") +
  scale_y_continuous(labels = label_scientific(digits = 2)) +
  theme(legend.position = "none",
        panel.spacing = unit(1, "lines",),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(size = 20),
        plot.margin = margin(c(0, 10, 10, 0))) +
  # Add correlation stats
  stat_cor(method = "kendall", label.x.npc = "left", label.y.npc = "top",
           p.accuracy = 0.001,
           family = "Helvetica",
           size = 5,
           lineheight = 0.9)

ggsave(micro_n_nocc_plot,
       filename = "output/2022/correlations/micro_n_nocc_plot.png",
       width = 10, height = 10, units = "in")

# Correlation between bacteria and C
# No cc
bact_c_nocc_stat_pre <- cor.test(
  dna_nc_nocc_pre$samp_c_med, dna_nc_nocc_pre$samp_med_bact, method = "kendall")
bact_c_noccstat_post <- cor.test(
  dna_nc_nocc_post$samp_c_med, dna_nc_nocc_post$samp_med_bact,
  method = "kendall")
# W cc
bact_c_wcc_stat_pre <- cor.test(
  dna_nc_wcc_pre$samp_c_med, dna_nc_wcc_pre$samp_med_bact, method = "kendall")
bact_c_wccstat_post <- cor.test(
  dna_nc_wcc_post$samp_c_med, dna_nc_wcc_post$samp_med_bact,
  method = "kendall")

# Correlation between fungi and N in pre vs post
# No cc
fung_c_nocc_stat_pre <- cor.test(
  dna_nc_nocc_pre$samp_c_med, dna_nc_nocc_pre$samp_med_fung, method = "kendall")
fung_c_nocc_stat_post <- cor.test(
  dna_nc_nocc_post$samp_c_med, dna_nc_nocc_post$samp_med_fung,
  method = "kendall")
# W cc
fung_c_wcc_stat_pre <- cor.test(
  dna_nc_wcc_pre$samp_c_med, dna_nc_wcc_pre$samp_med_fung, method = "kendall")
fung_c_wcc_stat_post <- cor.test(
  dna_nc_wcc_post$samp_c_med, dna_nc_wcc_post$samp_med_fung,
  method = "kendall")

# 4x4 Facet plot of bacteria and fungi vs %C in pre vs post wet
micro_c_nocc_plot <- dna_nc_nocc_prepost %>%
  mutate(pre_post_wet =
           # Terrible hack to reorder facets
           case_when(str_detect(pre_post_wet, "pre") ~ "a_pre",
                     str_detect(pre_post_wet, "post") ~ "b_post")) %>%
  pivot_longer(cols = c(samp_med_bact, samp_med_fung),
               names_to = "type", values_to = "med") %>%
  mutate(med = med * 1e6) %>%
  ggplot(aes(x = samp_c_med,
             y = med)) +
  geom_point(aes(fill = factor(type),
                 color = factor(type))) +
  geom_smooth(method = "lm", formula = y ~ x,
              aes(fill = type, color = factor(type)), alpha = 0.25) +
  facet_grid(type ~ pre_post_wet,
             scales = "free",
             labeller = (labeller(type = facet_type_labels,
                                  pre_post_wet = facet_prepost_labels))) +
  scale_fill_manual(values = c("#097CB2", "#03CC23")) +
  scale_color_manual(values = c("#16B4FF", "#39B708")) +
  labs(y = "DNA (Proportional Concentration)",
       x = "% Carbon",
       title = "Carbon") +
  theme(legend.position = "none",
        panel.spacing = unit(1, "lines"),
        plot.title = element_text(size = 20),
        plot.margin = margin(c(0, 10, 10, 10))) +
  # Add correlation stats
  stat_cor(method = "kendall", label.x.npc = "left", label.y.npc = "top",
           p.accuracy = 0.001,
           family = "Helvetica",
           size = 5,
           lineheight = 0.9)
ggsave(micro_c_nocc_plot,
       filename = "output/2022/correlations/micro_c_nocc_plot.png",
       width = 10, height = 10, units = "in")

# Combine 4 x 4 C and N plots
combined_cn_plot <- (micro_c_nocc_plot + micro_n_nocc_plot) +
  plot_annotation(title = paste0("DNA Quantities vs Total Soil CN",
  " in Rewet Soils Without Cover Crop"),
  theme = theme(plot.title = element_text(size = 32)))
ggsave(combined_cn_plot,
       filename = "output/2022/correlations/micro_cn_nocc_plot.png",
       width = 22, height = 12, units =)


# Correlation between bacteria and C:N #
bact_cn_stat <- cor.test(
  dna_nc$samp_cn_med, dna_nc$samp_med_bact, method = "kendall")
# Correlation between fungi and C:N #
fung_cn_stat <- cor.test(
  dna_nc$samp_cn_med, dna_nc$samp_med_fung, method = "kendall")

# Side by side plot of bacteria and fungi vs C:N
micro_cn_plot <- dna_nc %>%
  pivot_longer(cols = c(samp_med_bact, samp_med_fung),
               names_to = "type", values_to = "med") %>%
  ggplot(aes(x = samp_cn_med,
             y = med,
             fill = factor(type),
             color = factor(type))) +
  geom_point(aes(color = factor(type))) +
  geom_smooth(method = "lm", formula = y ~ x,
              aes(fill = type, color = factor(type)), alpha = 0.25) +
  facet_wrap(~ factor(type),
             scales = "free", labeller = facet_type_labels,
             dir = "v") +
  scale_fill_manual(values = c("#097CB2", "#03CC23")) +
  scale_color_manual(values = c("#16B4FF", "#39B708")) +
  labs(y = "DNA (Proportional Concentration)",
       x = "C:N Ratio",
       title = "DNA Quantities vs C:N Ratios in Rewet Soils") +
  scale_y_continuous(labels = label_scientific(digits = 2)) +
  theme(legend.position = "none",
        panel.spacing = unit(1, "lines"))
ggsave(micro_n_plot,
       filename = "output/2022/correlations/micro_n_plot.png",
       width = 10, height = 10, units = "in")


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

#### Correlation between fungi and bacteria ####
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


ggscatter(dna_nc, x = "samp_med_fung", y = "samp_cn_med",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "kendall")

dna_nc_cw <- dna_nc %>%
  filter(pre_post_wet == "cw")
ggscatter(dna_nc_pre, x = "samp_med_fung", y = "samp_med_bact",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "kendall")
ggscatter(dna_nc_post, x = "samp_med_fung", y = "samp_med_bact",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "kendall")
ggscatter(dna_nc_cw, x = "samp_med_fung", y = "samp_med_bact",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "kendall")

dna_co2_nocc <- dna_co2 %>%
  filter(cc_treatment == "no_cc")
ggscatter(dna_co2_nocc, x = "samp_med_fung", y = "co2_med",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "kendall")

dna_co2_wcc <- dna_co2 %>%
  filter(cc_treatment == "w_cc")
ggscatter(dna_co2_wcc, x = "samp_med_fung", y = "co2_med",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "kendall")
