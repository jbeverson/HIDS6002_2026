# from dataviz ch. 19 (common pitfalls of color use)
fig_popgrowth_colored <- function() {
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


  colors <- c(rainbow_hcl(8, l = 35, c = 25, start = 0, end = 315),
              rainbow_hcl(8, l = 45, c = 34, start = -10, end = 305),
              rainbow_hcl(9, l = 55, c = 42, start = -20, end = 300),
              rainbow_hcl(9, l = 65, c = 50, start = -30, end = 290),
              rainbow_hcl(9, l = 75, c = 55, start = -40, end = 280),
              rainbow_hcl(8, l = 85, c = 32, start = -50, end = 265))

  #colors <- sample(colors, 51)

  p_base <- ggplot(popgrowth_df, aes(x = pop2000, y = popgrowth, color = as.character(state))) +
    geom_point(size = 4) +
    scale_x_log10(labels = label_log10) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_color_manual(values = colors, name = "state") +
    xlab("population size in 2000") +
    ylab("population growth\n2000 to 2010   ") +
    theme_dviz_grid() +
    theme(
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 12),
      legend.justification = "center",
      legend.box.margin = margin(14, 0, 28, 0),
      legend.spacing.y = unit(2, "pt"),
      plot.margin = margin(14, 14, 14, 1.5)
    )

  p_comb <- plot_grid(p_base + theme(legend.position = "none"), get_legend(p_base), ncol = 1)
  stamp_bad(p_comb)
}
