# script to plot multiple microbial DNA plots after adding pairwise comparisons
# Initial draft by Naupaka Zimmerman November 17, 2022 nzimmerman@usfca.edu

# load packages
library("readr")
library("dplyr")
library("ggplot2")
library("scales") # for plot label niceness
library("forcats") # for factor manipulation
library("tidyverse")
library("grid")
library("egg")
library("ggsignif")

# source functions and themes, see those files for comments etc
source("code/functions/set_plot_themes.R")
set_theme("pres")

#### BACTERIA + FUNGI ####

# Read in data, subset to bacterial and pre/post-wet, no cc only
dna_all <- read_csv("data/cleaned_data/qPCR/samp_medians.csv") %>%
  mutate(samp_med_bact_human_read = samp_med_bact * 1e6) %>%
  mutate(samp_med_fung_human_read = samp_med_fung * 1e6)

# Filter for only no cc, pre/post bacterial DNA data
nocc_rewet <- dna_all %>%
  filter(cc_treatment == "no_cc") %>%
  filter(drying_treatment == "one_wk" |
           drying_treatment == "two_wk" |
           drying_treatment == "four_wk") %>%
  filter(pre_post_wet == "pre" |
           pre_post_wet == "post")

# Filter for only w cc, pre/post bacterial DNA data
wcc_rewet <- dna_all %>%
  filter(cc_treatment == "w_cc") %>%
  filter(drying_treatment == "one_wk" |
           drying_treatment == "two_wk" |
           drying_treatment == "four_wk") %>%
  filter(pre_post_wet == "pre" |
           pre_post_wet == "post")

# Set facet labels
facet_drying_labels <- as_labeller(c("one_wk" = "One Week",
                                     "two_wk" = "Two Weeks",
                                     "four_wk" = "Four Weeks"))
# Create Wilcoxon annotation
wilcox_annot <- data.frame(drying_treatment = "four_wk",
                           label = "Pairwise Wilcoxon \nRanked Sum Tests")

# Create function to not show NS in ggsignif plots
sigFunc <- function(x){
  if (x < 0.001){"***"}
  else if (x < 0.01){"**"}
  else if (x < 0.05){"*"}
  else if (x < 0.1){"."}
  else {NA}}

# Plot no cc bact using ggsignif and unpaired Wilcoxon Ranked Sum Tests
nocc_bact_rewet_plot <- nocc_rewet %>%
  ggplot(aes(x = factor(pre_post_wet, levels = c("pre", "post")),
             y = samp_med_bact_human_read)) +
  geom_boxplot(aes(fill = pre_post_wet,
                   color = pre_post_wet)) +
  facet_wrap(~ factor(drying_treatment,
                      levels = c("one_wk", "two_wk", "four_wk")),
             labeller = facet_drying_labels) +
  scale_x_discrete(labels = c("Pre-Wet", "Post-Wet")) +
  scale_fill_manual(name = NULL, limits = c("pre", "post"),
                    values = c("#16B4FF", "#34980D"),
                    labels = c("Pre-Wet", "Post-Wet")) +
  scale_color_manual(name = NULL, limits = c("pre", "post"),
                     values = c("#097CB2", "#195004"),
                     labels = c("Pre-Wet", "Post-Wet")) +
  scale_y_continuous(limits = c(25, 70)) +
  theme(legend.position = "none",
        strip.text = element_text(size = 12)) +
  labs(x = element_blank(),
       y = "Proportional DNA Amount",
       title = "Bacterial DNA Changes Without Cover Crop") +
  # Adds Wilcoxon pairwise comparisons
  geom_signif(comparisons = list(c("pre", "post")), test = "wilcox.test",
              map_signif_level = sigFunc,
              y_position = 64, family = "Helvetica", textsize = 6) +
  # Add test annotation
  geom_text(x = 1.5, y = 6.6, data = wilcox_annot, aes(label = label,
                                                     family = "Helvetica",
                                                     size = 16,
                                                     lineheight = 0.9))
ggsave(nocc_bact_rewet_plot,
       filename = "output/2022/qpcr_plots/bact_rewet_no_cc_.png",
       width = 10, height = 8, units = "in")

# Plot w cc bact using ggsignif and unpaired Wilcoxon Ranked Sum Tests
wcc_bact_rewet_plot <- wcc_rewet %>%
  ggplot(aes(x = factor(pre_post_wet, levels = c("pre", "post")),
             y = samp_med_bact_human_read)) +
  geom_boxplot(aes(fill = pre_post_wet,
                   color = pre_post_wet)) +
  facet_wrap(~ factor(drying_treatment,
                      levels = c("one_wk", "two_wk", "four_wk")),
             labeller = facet_drying_labels) +
  scale_x_discrete(labels = c("Pre-Wet", "Post-Wet")) +
  scale_fill_manual(name = NULL, limits = c("pre", "post"),
                    values = c("#16B4FF", "#34980D"),
                    labels = c("Pre-Wet", "Post-Wet")) +
  scale_color_manual(name = NULL, limits = c("pre", "post"),
                     values = c("#097CB2", "#195004"),
                     labels = c("Pre-Wet", "Post-Wet")) +
  scale_y_continuous(limits = c(40, 225)) +
  theme(legend.position = "none",
        strip.text = element_text(size = 12)) +
  labs(y = "Proportional DNA Amount",
       x = element_blank(),
       title = "Bacterial DNA Changes With Cover Crop") +
  # Adds Wilcoxon pairwise comparisons
  geom_signif(comparisons = list(c("pre", "post")), test = "wilcox.test",
              map_signif_level = sigFunc, family = "Helvetica", textsize = 6) +
  # Add test annotation
  geom_text(x = 1.5, y = 210, data = wilcox_annot, aes(label = label,
                                                       family = "Helvetica",
                                                       size = 16,
                                                       lineheight = 0.9))
ggsave(wcc_bact_rewet_plot,
       filename = "output/2022/qpcr_plots/bact_rewet_w_cc_.png",
       width = 10, height = 8, units = "in")

# Plot w cc fung (no significance in no cc fung) using ggsignif
# and unpaired Wilcoxon Ranked Sum Tests
wcc_fung_rewet_plot <- wcc_rewet %>%
  ggplot(aes(x = factor(pre_post_wet, levels = c("pre", "post")),
             y = samp_med_fung_human_read)) +
  geom_boxplot(aes(fill = pre_post_wet,
                   color = pre_post_wet)) +
  facet_wrap(~ factor(drying_treatment,
                      levels = c("one_wk", "two_wk", "four_wk")),
             labeller = facet_drying_labels) +
  scale_x_discrete(labels = c("Pre-Wet", "Post-Wet")) +
  scale_fill_manual(name = NULL, limits = c("pre", "post"),
                    values = c("#16B4FF", "#34980D"),
                    labels = c("Pre-Wet", "Post-Wet")) +
  scale_color_manual(name = NULL, limits = c("pre", "post"),
                     values = c("#097CB2", "#195004"),
                     labels = c("Pre-Wet", "Post-Wet")) +
  scale_y_continuous(limits = c(0, 4.7)) +
  theme(legend.position = "none",
        strip.text = element_text(size = 12)) +
  labs(y = "Proportional DNA Amount",
       x = element_blank(),
       title = "Fungal DNA Changes With Cover Crop") +
  # Adds Wilcoxon pairwise comparisons
  geom_signif(comparisons = list(c("pre", "post")), test = "wilcox.test",
              map_signif_level = sigFunc,
              family = "Helvetica", textsize = 6) +
  # Add test annotation
  geom_text(x = 1.5, y = 4.3, data = wilcox_annot, aes(label = label,
                                                      family = "Helvetica",
                                                      size = 16,
                                                      lineheight = 0.9))
ggsave(wcc_fung_rewet_plot,
       filename = "output/2022/qpcr_plots/fung_rewet_w_cc_.png",
       width = 10, height = 8, units = "in")

grouped_ggbetweenstats(data = wcc_rewet, grouping.var = drying_treatment,
                       x = pre_post_wet,
                       y = samp_med_fung_human_read,
                       plot.type = "box", p.adjust.method = "none",
                       type = "nonparametric", pairwise.display = "sig")
