# This function calculates statistics per week for differences between
# w cc and without cc samples in the N type inputted as an argument

# Sarah Gao
# November 1, 2022

library("dplyr")

wk_stats <- function(n_data, y_var) {

  # Run statistics per week
  initial <- n_data %>%
    filter(drying_treatment == "initial") %>%
    kruskal.test(data = ., get(y_var) ~ cc_treatment)
  one <- n_data %>%
    filter(drying_treatment == "one_wk") %>%
    kruskal.test(data = ., get(y_var) ~ cc_treatment)
  two <- n_data %>%
    filter(drying_treatment == "two_wk") %>%
    kruskal.test(data = ., get(y_var) ~ cc_treatment)
  four <- n_data %>%
    filter(drying_treatment == "four_wk") %>%
    kruskal.test(data = ., get(y_var) ~ cc_treatment)

  wk_sum <- data.frame("drying_treatment" = c("initial", "one_wk", "two_wk",
                                              "four_wk"),
                       "chi_sq" = c(initial$statistic,
                                    one$statistic,
                                    two$statistic,
                                    four$statistic),
                       "df" = c(initial$parameter,
                                one$parameter,
                                two$parameter,
                                four$parameter),
                       "p_value" = c(initial$p.value,
                                     one$p.value,
                                     two$p.value,
                                     four$p.value))

  return(wk_sum)
}
