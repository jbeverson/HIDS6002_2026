# from dataviz ch. 19 (common pitfalls of color use)
fig_popgrowth_labeled <- function() {
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


  library(ggrepel)

  set.seed(7586)
  region_colors <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442")

  labeled_states <- c(
    "Alaska", "Arizona", "California", "Florida", "Wisconsin", 
    "Louisiana", "Nevada", "Michigan", "Montana", "New Mexico",
    "Pennsylvania", "New York", "Oregon", "Rhode Island",
    "Tennessee", "Texas", "Utah", "Vermont"
  )

  df_repel <- select(popgrowth_df, x = pop2000, y = popgrowth, state) %>%
    mutate(label = ifelse(state %in% labeled_states, as.character(state), ""))

  ggplot(popgrowth_df, aes(x = pop2000, y = popgrowth, color = region, fill = region)) +
    geom_text_repel(
      data = df_repel,
      aes(x, y, label = label),
      segment.alpha = 0.5, point.padding = 0.25,
      box.padding = .8,
      force = 1,
      min.segment.length = 0.1,
      family = dviz_font_family,
      size = 11/.pt, inherit.aes = FALSE
    ) +
    geom_point(size = 3.5, color = "white") +
    geom_point(size = 2.5, shape = 21) +
    scale_x_log10(labels = label_log10) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_fill_manual(values = region_colors) +
    scale_color_manual(values = darken(region_colors, .3)) +
    xlab("population size in 2000") +
    ylab("population growth, 2000 to 2010") +
    theme_dviz_grid()
}
