# from dataviz ch. 19 (common pitfalls of color use)
fig_piyg_cvd <- function() {
  grad_red_green <- gg_color_swatches(
    n = 5,
    plot_margin = margin(17.5, 1, 0, 1),
    xmargin = 0.1,
    ymargin = 0.05
  ) + 
    scale_fill_brewer(type = "div", palette = "PiYG")

  cvd_sim(grad_red_green, label_y = 0.96)
}
