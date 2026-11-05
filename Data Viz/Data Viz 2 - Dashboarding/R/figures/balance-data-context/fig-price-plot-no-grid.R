# from dataviz ch. 23 (balance the data and the context)
fig_price_plot_no_grid <- function() {
  tech_stocks <- tech_stocks_clean()
  price_plot <- ggplot(tech_stocks, aes(x=date, y=price_indexed, color=ticker)) +
    geom_line(size = 0.66, na.rm = TRUE) +
    scale_color_manual(
      values = c("#000000", "#E69F00", "#56B4E9", "#009E73"),
      name = NULL,
      breaks = c("FB", "GOOG", "MSFT", "AAPL"),
      labels = c("Facebook", "Alphabet", "Microsoft", "Apple")
    ) +
    scale_x_date(
      name="year",
      limits=c(ymd("2012-06-01"), ymd("2017-05-31")),
      expand=c(0,0)
    ) + 
    scale_y_continuous(
      name = "stock price, indexed",
      limits = c(0, 560),
      expand = c(0,0)
    ) +
    theme_dviz_grid() +
    theme(
      panel.background = element_rect(fill = "grey90"),
      panel.grid.major = element_line(color = "white"),
      panel.grid.minor = element_line(color = "white", size = rel(0.5)),
      legend.key = element_rect(fill = "grey90", color = "white"),
      axis.ticks = element_line(color = "black"),
      plot.margin = margin(7, 7, 3, 1.5)
    )

  stamp_bad(
    price_plot + 
      theme_dviz_open() +
      theme(plot.margin = margin(7, 7, 3, 1.5))
  )
}
