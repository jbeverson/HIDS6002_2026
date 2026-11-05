# from dataviz ch. 22 (titles, captions, and tables)
fig_blue_jays_bubbles <- function() {
  blue_jays$sex <- ifelse(blue_jays$KnownSex == "F", "female birds", "male birds")
  blue_jays$sex <- factor(blue_jays$sex, levels = c("male birds", "female birds"))
  ggplot(blue_jays, aes(Mass, Head, size = Skull, fill = KnownSex)) + 
    geom_point(pch = 21, color = "white") +
    scale_x_continuous(name = "body mass (g)") +
    scale_y_continuous(name = "head length (mm)", breaks = c(52, 54, 56, 58, 60)) +
    scale_fill_manual(
      values = c(F = "#D55E00", M = "#0072B2"),
      labels = c("female   ", "male"),
      name = "sex",
      guide = guide_legend(
        direction = "horizontal",
        title.position = "top",
        title.hjust = 0.5,
        label.position = "right",
        keyheight = grid::unit(19, "pt"),
        order = 1,
        override.aes = list(size = 4)
      )
    ) +
    scale_radius(
      name = "skull size (mm)",
      range = c(2, 7),
      limits = c(28, 34),
      breaks = c(28, 30, 32, 34),
      labels = c("28   ", "30   ", "32   ", "34"),
      guide = guide_legend(
        direction = "horizontal",
        title.position = "top",
        title.hjust = 0.5,
        label.position = "right",
        order = 2,
        override.aes = list(fill = "gray40")
      )
    ) +
    theme_dviz_grid() +
    theme(
      legend.margin = margin(0, 0, 0, 20),
      legend.position = "top",
      legend.box = "horizontal",
      legend.box.spacing = grid::unit(0, "pt"),
      legend.justification = c(1, 0),
      legend.spacing.x = unit(2, "pt"),
      legend.spacing.y = unit(2, "pt"),
      legend.background = element_rect(fill = "white", color = NA),
      legend.key.width = unit(10, "pt")
    )
}
