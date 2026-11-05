# from dataviz ch. 17 (proportional ink)
fig_oceania_gdp_dots <- function() {
  library(gapminder)

  df_oceania <- filter(gapminder_unfiltered, year == 2007, continent == "Oceania") %>%
    mutate(GDP = pop*gdpPercap) %>%
    arrange(desc(GDP))

  ggplot(df_oceania, aes(x = reorder(country, -GDP), y = log10(GDP))) + 
    geom_point(size = 3.5, color = "#0072B2") + 
    geom_label(aes(label = country, y = log10(GDP) - .08), hjust = 1, size = 12/.pt,
               family = dviz_font_family,
               fill = "white", alpha = 0.5, label.padding = grid::unit(2, "pt"),
               label.r = grid::unit(0, "pt"), label.size = 0) +
    scale_y_continuous(breaks = log10(c(3e8, 1e9, 3.e9, 1e10, 3.e10, 1e11, 3.e11, 1e12)),
                       labels = c("0.3", "1.0", "3.0", "10", "30", "100", "300", "1000"),
                       name = "              GDP (billion USD)",
                       limits = log10(c(3e7, 9.9e11)),
                       expand = c(0, 0)) +
    scale_x_discrete(name = NULL, breaks = NULL) +
    coord_flip() +
    theme_dviz_vgrid(12, rel_small = 1) +
    theme(plot.margin = margin(12, 6, 3, 1.5))
}
