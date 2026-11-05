# from dataviz ch. 22 (titles, captions, and tables)
fig_tech_stocks_minimal_labeling <- function() {
  tech_stocks <- tech_stocks_clean()
  price_plot_base <- ggplot(tech_stocks, aes(x = date, y = price_indexed, color = ticker)) +
    geom_line(na.rm = TRUE) +
    scale_color_manual(
      values = c("#000000", "#E69F00", "#56B4E9", "#009E73"),
      name = "",
      breaks = c("FB", "GOOG", "MSFT", "AAPL"),
      labels = c("Facebook", "Alphabet", "Microsoft", "Apple")
    ) +
    scale_x_date(
      limits = c(ymd("2012-06-01"), ymd("2017-05-31")),
      expand = c(0,0)
    ) + 
    scale_y_continuous(
      limits = c(0, 560),
      expand = c(0,0)
    ) +
    theme_dviz_hgrid() + 
    theme(plot.margin = margin(3, 7, 3, 1.5))

  price_plot_base + xlab(NULL) + ylab("stock price, indexed")
}
