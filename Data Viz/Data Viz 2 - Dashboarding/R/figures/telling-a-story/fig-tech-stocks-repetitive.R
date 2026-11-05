# from dataviz ch. 29 (telling a story and making a point)
fig_tech_stocks_repetitive <- function() {
  tech_stocks <- tech_stocks_clean()
  final_prices <- filter(tech_stocks, date == ymd("2017-05-31"))

  fb_price_plot <- ggplot(
    filter(tech_stocks, ticker == "FB"),
    aes(x = date, y = price, color = ticker)
  ) +
    geom_line(size = 0.66, color = "#0072B2", na.rm = TRUE) +
    scale_x_date(
      name = "year",
      limits = c(ymd("2012-06-01"), ymd("2017-05-31")),
      expand = c(0,0)
    ) + 
    scale_y_continuous(
      name = "stock price (USD)",
      limits = c(0, 155),
      expand = c(0,0),
      sec.axis = dup_axis(
        breaks = filter(final_prices, ticker == "FB")$price,
        labels = "Facebook",
        name = NULL
      )
    ) +
    theme_dviz_hgrid() + 
    theme(axis.ticks.y.right = element_blank())

  all_indexed_price_plot <- ggplot(tech_stocks, aes(x = date, y = price_indexed, color = ticker)) +
    geom_line(size = 0.66, na.rm = TRUE) +
    scale_color_manual(
      values = c("#000000", "#0072B2", "#E69F00", "#009E73"),
      guide = "none"
    ) +
    scale_x_date(
      name = "year",
      limits = c(ymd("2012-06-01"), ymd("2017-05-31")),
      expand = c(0,0)
    ) + 
    scale_y_continuous(
      name = "stock price, indexed",
      limits = c(0, 560),
      expand = c(0,0),
      sec.axis = dup_axis(
        breaks = final_prices$price_indexed,
        labels = final_prices$company,
        name = NULL
      )
    ) +
    theme_dviz_hgrid() + 
    theme(axis.ticks.y.right = element_blank())

  stamp_ugly(
    plot_grid(
      fb_price_plot,
      all_indexed_price_plot,
      ncol = 1, align = "v", labels = "auto"
    ) + theme(plot.margin = margin(14, 7, 0, 1.5))
  )
}
