# from dataviz ch. 17 (proportional ink)
fig_hawaii_income_change <- function() {
  hawaii_income_diff <- select(hawaii_income, county, year, median_income) %>%
    spread(year, median_income) %>%
    mutate(income_diff = `2015` - `2010`,
           income_ratio = `2015` / `2010`)

  ggplot(hawaii_income_diff, aes(x = reorder(county, desc(filter(hawaii_income, year == 2015)$median_income)),
                                 y = income_diff)) + 
    geom_col(fill = "#56B4E9") +
    xlab("county") +
    scale_y_continuous(
      name = "5-year change in median income (USD)", 
      limits = c(-5000, 25000),
      expand = c(0, 0),
      labels = c("-$5,000", "$0", "$5,000", "$10,000", "$15,000", "$20,000", "$25,000")
    ) +
    theme_dviz_hgrid() + 
    theme(axis.ticks.x = element_blank(),
          plot.margin = margin(7, 7, 3, 1.5))
}
