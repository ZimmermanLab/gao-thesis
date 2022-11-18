library("ggthemes")

# make a custom theme (starts with theme_few from ggthemes as baseline)
custom_few_theme <- function() {
  theme_few() +
    theme(text = element_text(size = 18))
}
