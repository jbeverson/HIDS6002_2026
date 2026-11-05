# from dataviz ch. 20 (redundant coding)
fig_iris_ellipses <- function() {
  breaks = c("setosa", "virginica", "versicolor")
  labels = paste0("Iris ", breaks)

  label_df <- data.frame(
    Species = c("setosa", "virginica", "versicolor"),
    label = c("Iris setosa", "Iris virginica", "Iris versicolor"),
    Sepal.Width = c(4.2, 3.76, 2.08),
    Sepal.Length = c(5.7, 7, 5.1),
    hjust = c(0, 0.5, 0),
    vjust = c(0, 0.5, 1))

  iris_scatter3 <- ggplot(iris, 
        aes(
          x = Sepal.Length,
          y = Sepal.Width,
          color = Species
        )
      ) + 
      geom_point(
        aes(shape = Species, fill = Species),
        size = 2.5,
        position = position_jitter(
          width = 0.01 * diff(range(iris$Sepal.Length)),
          height = 0.01 * diff(range(iris$Sepal.Width)),
          seed = 3942)
      ) +
      stat_ellipse(size = 0.5) +
      geom_text(
        data = label_df,
        aes(
          x = Sepal.Length, y = Sepal.Width, label = label, color = Species,
          hjust = hjust, vjust = vjust
        ),
        family = dviz_font_family,
        size = 14/.pt,
        fontface = "italic",
        inherit.aes = FALSE
      ) +
      scale_shape_manual(
        values = c(21, 22, 23),
        breaks = breaks,
        name = NULL
      ) +
      scale_fill_manual(
        values = c("#56B4E980", "#E69F0080", "#009E7380"),
        breaks = breaks,
        name = NULL
      ) +
      scale_color_manual(
        values = darken(c("#56B4E9", "#E69F00", "#009E73"), 0.3),
        breaks = breaks,
        name = NULL
      ) +
      guides(fill = "none", color = "none", shape = "none") +
      scale_x_continuous(
        limits = c(3.95, 8.2), expand = c(0, 0),
        labels = c("4.0", "5.0", "6.0", "7.0", "8.0"),
        name = "sepal length"
      ) +
      scale_y_continuous(
        limits = c(1.9, 4.6), expand = c(0, 0),
        name = "sepal width"
      ) +
      theme_dviz_open()

  iris_scatter3
}
