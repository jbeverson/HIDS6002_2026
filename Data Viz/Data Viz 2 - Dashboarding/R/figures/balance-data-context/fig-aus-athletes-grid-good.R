# from dataviz ch. 23 (balance the data and the context)
fig_aus_athletes_grid_good <- function() {
  male_Aus <- filter(Aus_athletes, sex=="m") %>%
      filter(sport %in% c("basketball", "field", "swimming", "track (400m)",
                          "track (sprint)", "water polo")) %>%
      mutate(sport = case_when(sport == "track (400m)" ~ "track",
                               sport == "track (sprint)" ~ "track",
                               TRUE ~ sport))

  male_Aus$sport <- factor(male_Aus$sport,
                           levels = c("field", "water polo", "basketball", "swimming", "track"))

  p_Aus_base <- ggplot(male_Aus, aes(x=height, y=pcBfat, color=sport, fill=sport, shape=sport)) +
    geom_point(size = 2.5) +
    scale_shape_manual(values = 21:25) +
    scale_color_OkabeIto(order=c(2, 1, 3, 4, 5), darken = 0.3) +
    scale_fill_OkabeIto(order=c(2, 1, 3, 4, 5), darken = 0.1, alpha = 0.7) +
    scale_x_continuous(limits = c(169, 210), name = "height (cm)") +
    scale_y_continuous(limits = c(5, 20), name = "% body fat")

  p_Aus_base + 
    theme_dviz_grid()# + panel_border(colour = "grey70") + theme(axis.ticks = element_line(colour = "grey70"))
}
