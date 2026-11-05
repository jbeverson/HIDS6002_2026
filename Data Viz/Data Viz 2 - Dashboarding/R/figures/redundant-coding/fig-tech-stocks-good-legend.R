# from dataviz ch. 20 (redundant coding)
fig_tech_stocks_good_legend <- function() {
  tech_stocks <- tech_stocks_clean()
  price_plot_base_good <- ggplot(tech_stocks, aes(x = date, y = price_indexed, color = ticker)) +
    scale_color_manual(
      values = c("#000000", "#E69F00", "#56B4E9", "#009E73"),
      name = "",
      breaks = c("FB", "GOOG", "MSFT", "AAPL"),
      labels = c("Facebook", "Alphabet", "Microsoft", "Apple")
    ) +
    scale_x_date(
      name = "year",
      limits = c(ymd("2012-06-01"), ymd("2017-05-31")),
      expand = c(0,0)
    ) + 
    scale_y_continuous(
      name = "stock price, indexed",
      limits = c(0, 560),
      expand = c(0,0)
    )

  price_plot_base_good +
    geom_line(size = 0.66, na.rm = TRUE) +
    theme_dviz_hgrid() + 
    theme(plot.margin = margin(3, 7, 3, 1.5))
}
