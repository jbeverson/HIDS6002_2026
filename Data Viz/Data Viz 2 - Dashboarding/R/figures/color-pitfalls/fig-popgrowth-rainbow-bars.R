# from dataviz ch. 19 (common pitfalls of color use)
fig_popgrowth_rainbow_bars <- function() {
  popgrowth_df <- left_join(US_census, US_regions) %>%
      group_by(region, division, state) %>%
      summarize(pop2000 = sum(pop2000, na.rm = TRUE),
                pop2010 = sum(pop2010, na.rm = TRUE),
                popgrowth = (pop2010-pop2000)/pop2000,
                area = sum(area)) %>%
      arrange(popgrowth) %>%
      ungroup() %>%
      mutate(state = factor(state, levels = state),
             region = factor(region, levels = c("West", "South", "Midwest", "Northeast")))


  popgrowth_bars_rainbow <- ggplot(popgrowth_df, aes(x = state, y = 100*popgrowth, fill = state)) + 
    geom_col() + 
    scale_y_continuous(
      limits = c(-.6, 37.5), expand = c(0, 0),
      labels = scales::percent_format(accuracy = 1, scale = 1),
      name = "population growth, 2000 to 2010"
    ) +
    scale_fill_hue(c = 140, l = 55) +
    coord_flip() + 
    theme_dviz_vgrid(12, rel_small = 1) +
    theme(axis.title.y = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.length = unit(0, "pt"),
          axis.text.y = element_text(size = 10),
          legend.position = "none",
          plot.margin = margin(18, 1.5, 3, 1.5))

  stamp_ugly(popgrowth_bars_rainbow)
}
