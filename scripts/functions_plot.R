# ___________________________________________________________________
# ___________________________________________________________________
# interpQM: Homogenization of snow depth data
# Plot-functions ----
# Author: Gernot Resch
# Date: 26.04.2024
# ___________________________________________________________________
# ___________________________________________________________________

theme_minimal <- theme_minimal(
  base_size = 9
) +
  theme(
    plot.background = element_rect(fill = "white",
                                   color = "white"),
    plot.title.position = "plot",
    legend.position = "bottom",
    text = element_text(face = "bold"),
    panel.grid = element_line(linetype = "dotted"),
    plot.caption = element_text(size = rel(0.4)),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey90",
                                    color = "grey30",
                                    linewidth = 0),
    panel.border = element_rect(color = "grey30",
                                fill = NA,
                                linewidth = 0.5),
    plot.title = ggtext::element_markdown(size = 12),
    plot.subtitle = ggtext::element_markdown(size = 10)
  )
