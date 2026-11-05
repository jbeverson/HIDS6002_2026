# from dataviz ch. 20 (redundant coding)
fig_iris_three_shapes_cvd <- function() {
  breaks = c("setosa", "virginica", "versicolor")
  labels = paste0("Iris ", breaks)

  iris_scatter2_base <- ggplot(
    iris, aes(x = Sepal.Length, y = Sepal.Width, shape = Species, fill = Species, color = Species)
  ) +     
      scale_shape_manual(
        values = c(21, 22, 23),
        breaks = breaks,
        labels = labels,
        name = NULL
      ) +
      scale_color_manual(
        values = darken(c("#56B4E9", "#E69F00", "#009E73"), 0.3),
        breaks = breaks,
        labels = labels,
        name = NULL
      ) +
      scale_fill_manual(
        values = c("#56B4E980", "#E69F0080", "#009E7380"),
        breaks = breaks,
        labels = labels,
        name = NULL
      ) +
      scale_x_continuous(
        limits = c(3.95, 8.2), expand = c(0, 0),
        labels = c("4.0", "5.0", "6.0", "7.0", "8.0"),
        name = "sepal length"
      ) +
      scale_y_continuous(
        limits = c(1.9, 4.6), expand = c(0, 0),
        name = "sepal width"
      )

  iris_scatter2_small <- iris_scatter2_base +
    geom_point(
      size=.655*2.5, stroke = .655*0.5,
      position = position_jitter(
        width = 0.01 * diff(range(iris$Sepal.Length)),
        height = 0.01 * diff(range(iris$Sepal.Width)),
        seed = 3942)
    ) +
    theme_dviz_grid(
     .655*14,
      line_size = .85*.5 # make line size a little bigger than mathematically correct
    ) + 
    theme(
      legend.title.align = 0.5,
      legend.text = element_text(face = "italic"),
      legend.spacing.y = grid::unit(.655*3, "pt"),
      plot.margin = margin(18, 1, 1, 1)
    )

  cvd_sim2(iris_scatter2_small, label_size = 14, label_y = .98, scale = .95)
}
