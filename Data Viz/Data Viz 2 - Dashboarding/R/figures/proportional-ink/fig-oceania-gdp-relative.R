# from dataviz ch. 17 (proportional ink)
fig_oceania_gdp_relative <- function() {
  library(gapminder)

  df_oceania <- filter(gapminder_unfiltered, year == 2007, continent == "Oceania") %>%
    mutate(GDP = pop*gdpPercap) %>%
    arrange(desc(GDP))

  GDP_PNG <- filter(df_oceania, country == "Papua New Guinea")$GDP

  df_oceania_ratios <- mutate(df_oceania, gdp_ratio = GDP/GDP_PNG) %>%
    filter(country != "Papua New Guinea")

  ggplot(df_oceania_ratios, aes(x = reorder(country, gdp_ratio), y = gdp_ratio)) + 
    geom_col(fill = "#56B4E9") + 
    scale_y_log10(breaks = c(1/16, 1/8, 1/4, 1/2, 1, 2, 4, 8, 16, 32, 64),
                  labels = c("1/16", "1/8", "1/4", "1/2", "1", "2", "4", "8", "16", "32", "64"),
                  name = "GDP relative to Papua New Guinea",
                  limits = c(.055, 70),
                  expand = c(0, 0)) +
    scale_x_discrete(name = NULL) +
    coord_flip() +
    theme_dviz_vgrid(12, rel_small = 1) +
    theme(axis.line = element_blank(),
          axis.ticks.length = grid::unit(0, "pt"),
          axis.ticks.y = element_blank(),
          plot.margin = margin(12, 6, 3, 1.5))
}
