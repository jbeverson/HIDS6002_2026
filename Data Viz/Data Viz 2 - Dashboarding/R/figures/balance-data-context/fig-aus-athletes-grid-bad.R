# from dataviz ch. 23 (balance the data and the context)
fig_aus_athletes_grid_bad <- function() {
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

  p <- p_Aus_base +
    theme_dviz_open() +
    theme(
      axis.line = element_blank(),
      panel.grid.major = element_line(color = "black", size = 0.3),
      panel.grid.minor = element_line(color = "black", size = 0.15),
      panel.border = element_rect(color = "black", size = 1),
      legend.background = element_rect(color = "black", size = 0.5),
      legend.margin = margin(7, 7, 7, 7),
      plot.background = element_rect(color = "black", size = 1),
      plot.margin = margin(7, 7, 7, 7)
    )

  # width: 6 in = 433.62pt
  # height: 0.618 * 6 in = 3.71 in = 268.12

  stamp_ugly(ggdraw() + draw_plot(p, x = 1/434, y = 1/268, width = 426/434, height = 252/268))
}
