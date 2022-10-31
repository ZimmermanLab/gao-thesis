# This function sets plot themes across all figures

# Sarah Gao
# October 29, 2022
# hellosarahgao@gmail.com

library("ggplot2")
library("extrafont")

# Set plot themes
set_theme <- function() {
  theme_update(
    text = element_text(family = "Georgia"),
    plot.title = element_text(face = "bold",
                              size = 20,
                              hjust = 0.5,
                              margin = margin(15, 0, 20, 0),
                              lineheight = 1.2),
    axis.title.x = element_text(size = 12,
                                face = "bold",
                                vjust = -3),
    axis.text.x = element_text(family = "Helvetica",
                               size = 10,
                               color = "#808080",
                               angle = 0, hjust = 0.5,
                               margin = margin(10, 0, 0, 0)),
    axis.title.y = element_text(size = 12,
                                face = "bold",
                                vjust = 3),
    axis.text.y = element_text(family = "Helvetica",
                               size = 10,
                               color = "#808080"),
    # These margins are for standalone figures
    plot.margin = margin(20, 30, 40, 30),
    # These margins are for poster assets
    # plot.margin = margin(0, 0, 10, 5),
    legend.position = "right",
    legend.title = element_blank(),
      # element_text(size = 10,
                  #              face = "bold",
                  #            color = "#000000",
                  #              margin = margin(r = 10, unit = "pt")),
    legend.text = element_text(family = "Helvetica",
                               size = 10,
                               color = "#808080",
                               margin = margin(10, 0, 10, 0)),
    legend.background = element_rect(color = "#E7E7E7"),
    legend.margin = margin(8, 8, 8, 8),
    legend.box.spacing = unit(20, "pt"),
    legend.key.size = unit(10, "pt"),
    legend.spacing.y = unit(0, "pt")
)
}
