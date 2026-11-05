# from dataviz ch. 17 (proportional ink)
fig_hawaii_income_bars_bad <- function() {
  p_income_base <- ggplot(filter(hawaii_income, year == 2015), aes(x = reorder(county, desc(median_income)), y = median_income)) +
    geom_col(fill = "#56B4E9") +
    xlab("county") +
    theme_dviz_hgrid() +
    theme(
      axis.ticks.x = element_blank(),
      plot.margin = margin(3, 7, 3, 1.5)
    )

  p_income_bad <- p_income_base + 
    coord_cartesian(xlim = c(0.5, 5.55), ylim = c(50000, 75000), expand = FALSE) +
    scale_y_continuous(
      name = "median income (USD)", 
      breaks = 10000*(5:7),
      labels = function(x) paste0("$", scales::comma(x))
    )

  stamp_bad(p_income_bad)
}
