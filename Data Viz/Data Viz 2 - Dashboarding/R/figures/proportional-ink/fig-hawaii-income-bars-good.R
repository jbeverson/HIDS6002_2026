# from dataviz ch. 17 (proportional ink)
fig_hawaii_income_bars_good <- function() {
  p_income_base <- ggplot(filter(hawaii_income, year == 2015), aes(x = reorder(county, desc(median_income)), y = median_income)) +
    geom_col(fill = "#56B4E9") +
    xlab("county") +
    theme_dviz_hgrid() +
    theme(
      axis.ticks.x = element_blank(),
      plot.margin = margin(3, 7, 3, 1.5)
    )

  p_income_good <- p_income_base + 
    coord_cartesian(xlim = c(0.5, 5.55), ylim = c(0, 78000), expand = FALSE, clip = "off") +
    scale_y_continuous(
      name = "median income (USD)", 
      breaks = 20000*(0:3),
      labels = function(x) paste0("$", scales::comma(x))
    ) +
    theme(axis.line.x = element_blank())

  p_income_good
}
