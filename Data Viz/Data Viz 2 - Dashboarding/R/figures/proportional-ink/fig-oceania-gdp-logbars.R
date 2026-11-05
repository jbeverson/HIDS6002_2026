# from dataviz ch. 17 (proportional ink)
fig_oceania_gdp_logbars <- function() {
  library(gapminder)

  df_oceania <- filter(gapminder_unfiltered, year == 2007, continent == "Oceania") %>%
    mutate(GDP = pop*gdpPercap) %>%
    arrange(desc(GDP))

  oc_bad <- ggplot(df_oceania, aes(x = reorder(country, -GDP), y = log10(GDP))) + 
    geom_col(fill = "#56B4E9") + 
    scale_y_continuous(breaks = log10(c(3.1e8, 1e9, 3.e9, 1e10, 3.e10, 1e11, 3.e11, 1e12)),
                       labels = c("0.3", "1.0", "3.0", "10", "30", "100", "300", "1000"),
                       name = "GDP (billion USD)") +
    scale_x_discrete(name = NULL) +
    coord_flip(ylim = log10(c(3.1e8, 9.9e11)), expand = FALSE) +
    theme_dviz_vgrid(12, rel_small = 1) +
    theme(axis.ticks.y = element_blank(),
          plot.margin = margin(12, 6, 3, 1.5))

  stamp_bad(oc_bad)
}
