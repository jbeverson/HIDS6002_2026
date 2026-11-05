# from dataviz ch. 24 (use larger axis labels)
fig_aus_athletes_ugly <- function() {
  male_Aus <- filter(Aus_athletes, sex == "m") %>%
      filter(sport %in% c("basketball", "field", "swimming", "track (400m)",
                          "track (sprint)", "water polo")) %>%
      mutate(sport = case_when(sport == "track (400m)" ~ "track",
                               sport == "track (sprint)" ~ "track",
                               TRUE ~ sport))

  male_Aus$sport <- factor(male_Aus$sport,
                           levels = c("field", "water polo", "basketball", "swimming", "track"))

  p <- ggplot(male_Aus, aes(x = height, y = pcBfat, color = sport, fill = sport, shape = sport)) +
      scale_shape_manual(values = 21:25) +
      scale_color_OkabeIto(order = c(2, 1, 3, 4, 5), darken = 0.3) +
      scale_fill_OkabeIto(order = c(2, 1, 3, 4, 5), darken = 0.1, alpha = 0.7) +
      ylab("% body fat") +
      xlab("height (cm)")

  stamp_ugly(p + geom_point(size = 1.5) +
               theme_dviz_grid(10) + theme(plot.margin = margin(7, 7, 3, 1.5)))
}
