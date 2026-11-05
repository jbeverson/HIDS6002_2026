# from dataviz ch. 19 (common pitfalls of color use)
fig_colors_thin_lines <- function() {
  tiles_df <- data.frame(
    x = c(1, 2, 1, 2),
    y = c(1.75, 1.75, 1.25, 1.25),
    type = c("A", "B", "C", "D")
  )

  segments_df <- data.frame(
    x0 = rep(0.55, 4),
    x1 = rep(2.45, 4),
    y0 = seq(.9, .6, -.1),
    y1 = seq(.9, .6, -.1),
    type = c("A", "B", "C", "D")
  )

  points_df <- data.frame(
    x = rep(1.58 + .28*(0:3), 4),
    y = rep(seq(.4, .1, -.1), each = 4),
    size = rep(c(3, 2, 1, .5), 4),
    type = rep(c("A", "B", "C", "D"), each = 4)
  )

  p <- ggplot() + 
    geom_tile(data = tiles_df, aes(x, y, fill = type), width = 0.9, height = 0.45) +
    geom_segment(
      data = segments_df,
      aes(x = x0, xend = x1, y = y0, yend = y1, color = type),
      size = 1.5
    ) +
    geom_segment(
      data = segments_df,
      aes(x = x0, xend = x0 + .9, y = y0 - .5, yend = y1 - .5, color = type),
      size = .5
    ) +
    geom_point(data = points_df, aes(x, y, color = type, size = size)) +
    scale_fill_OkabeIto(order = c(1, 4, 2, 3)) +
    scale_color_OkabeIto(order = c(1, 4, 2, 3)) +
    scale_size_identity() +
    coord_cartesian(xlim = c(0.55, 2.45), ylim = c(0.05, 2.05), expand = FALSE) +
    theme_nothing() +
    theme(plot.margin = margin(14, 1, 0, 1))

  cvd_sim(p, label_y = 0.97)
}
