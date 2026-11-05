# from dataviz ch. 17 (proportional ink)
fig_fb_stock_drop_good <- function() {
  tech_stocks <- tech_stocks_clean()
  df_fb_drop <- filter(tech_stocks, ticker == "FB", date >= ymd("2016-10-22") & date < ymd("2017-01-22"))

  fb_drop_good <- ggplot(df_fb_drop, aes(x=date, height=price, y = 0)) +
    geom_ridgeline(alpha = 0.7) +
    scale_x_date(name = NULL, #name = "day",
                 breaks = ymd(c("2016-11-01", "2016-12-01", "2017-01-01")),
                 labels = c("Nov 1, 2016", "Dec 1, 2016", "Jan 1, 2017"),
                 expand=c(0,0)) + 
    scale_y_continuous(name="stock price (USD)",
                       limits = c(0, 150),
                       expand=c(0,0)) +
    theme_dviz_open() +
    background_grid(major = 'y', minor = 'none') +
    theme(plot.margin = margin(14, 7, 3, 1.5))

  stamp_phantom(fb_drop_good)
}
