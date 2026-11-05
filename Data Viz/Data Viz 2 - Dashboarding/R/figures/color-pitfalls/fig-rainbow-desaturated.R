# from dataviz ch. 19 (common pitfalls of color use)
fig_rainbow_desaturated <- function() {
  p1 <- gg_color_gradient(
    plot_margin = margin(17.5, 1, 0, 1),
    ymargin = 0.05
  ) + 
    scale_fill_gradientn(colors = rainbow(10))

  p2 <- gg_color_gradient(
    plot_margin = margin(17.5, 1, 0, 1),
    ymargin = 0.05
  ) + 
    scale_fill_gradientn(colors = desaturate(rainbow(10)))

  plot_grid(
    p1, NULL, p2,
    ncol = 1,
    rel_heights = c(0.9, 0.1, 0.9),
    labels = c("rainbow scale", "", "rainbow converted to grayscale"),
    label_x = 0, label_y = 0.96,
    label_size = 14
  )
}
