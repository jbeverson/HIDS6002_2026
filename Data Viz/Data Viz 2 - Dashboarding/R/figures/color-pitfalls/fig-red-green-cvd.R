# from dataviz ch. 19 (common pitfalls of color use)
fig_red_green_cvd <- function() {
  cols <- scales::colour_ramp(c("#FF1B1B", "#F9F1CE", high = "#057905"))(seq(0, 1, .25))

  grad_red_green <- gg_color_swatches(
    n = 5,
    plot_margin = margin(17.5, 1, 0, 1),
    xmargin = 0.1,
    ymargin = 0.05
  ) + 
    scale_fill_manual(values = cols)

  cvd_sim(grad_red_green, label_y = 0.96)
}
