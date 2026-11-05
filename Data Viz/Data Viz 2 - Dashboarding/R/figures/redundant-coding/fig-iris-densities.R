# from dataviz ch. 20 (redundant coding)
fig_iris_densities <- function() {
  # compute densities for sepal lengths
  iris_dens <- group_by(iris, Species) %>%
    do(ggplot2:::compute_density(.$Sepal.Length, NULL)) %>%
    rename(Sepal.Length = x)

  # get the maximum values
  iris_max <- filter(iris_dens, density == max(density)) %>%
    ungroup() %>%
    mutate(
      hjust = c(0, 0.4, 0),
      vjust = c(1, 0, 1),
      nudge_x = c(0.11, 0, 0.24),
      nudge_y = c(-0.02, 0.02, -0.02),
      label = paste0("Iris ", Species)
    )

  iris_p <- ggplot(iris_dens, aes(x = Sepal.Length, y = density, fill = Species, color = Species)) + 
    geom_density_line(stat = "identity") +
    geom_text(
      data = iris_max,
      aes(
        label = label, hjust = hjust, vjust = vjust, color = Species,
        x = Sepal.Length + nudge_x, 
        y = density + nudge_y
      ), 
      family = dviz_font_family,
      size = 14/.pt,
      inherit.aes = FALSE,
      fontface = "italic"
    ) +
    scale_color_manual(
      values = darken(c("#56B4E9", "#E69F00", "#009E73"), 0.3),
      breaks = c("virginica", "versicolor", "setosa"),
      guide = "none"
    ) +
    scale_fill_manual(
      values = c("#56B4E950", "#E69F0050", "#009E7350"),
      breaks = c("virginica", "versicolor", "setosa"),
      guide = "none"
    ) +
    scale_x_continuous(expand = c(0, 0), name = "sepal length") +
    scale_y_continuous(limits = c(0, 1.5), expand = c(0, 0)) +
    coord_cartesian(clip = "off") +
    theme_dviz_hgrid() +
    theme(
      axis.line.x = element_blank(),
      plot.margin = margin(6.5, 1.5, 3.5, 1.5)
    )
  
  iris_p
}
