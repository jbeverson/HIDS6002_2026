# from dataviz ch. 20 (redundant coding)
fig_tech_stocks_good_legend_cvd <- function() {
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

  price_plot_good_small <-
    price_plot_base_good + 
    geom_line(
      size = .85*0.66, # make line size a little bigger than mathematically correct
      na.rm = TRUE
    ) +
    theme_dviz_hgrid(
      .655*14,
      line_size = .85*.5 # make line size a little bigger than mathematically correct
    ) + 
    theme(plot.margin = margin(18, 1, 1, 1))

  cvd_sim2(price_plot_good_small, label_size = 14, label_y = .98, scale = .95)
}
