# from dataviz ch. 29 (telling a story and making a point)
fig_athletes_composite_repetitive <- function() {
  male_sport <- unique(filter(Aus_athletes, sex=="m")$sport)
  female_sport <- unique(filter(Aus_athletes, sex=="f")$sport)
  both_sport <- male_sport[male_sport %in% female_sport]
  athletes_df <- filter(Aus_athletes, sport %in% both_sport) %>%
    mutate(
      sport = case_when(
        sport == "track (400m)" ~ "track",
        sport == "track (sprint)" ~ "track",
        TRUE ~ sport
      ),
      sex = factor(sex, levels = c("f", "m"))
    )

  cc_df <- athletes_df %>%
    select(sex, rcc, wcc) %>%
    gather(variable, value, -sex) %>%
    group_by(sex, variable) %>%
    summarize(
      mean = mean(value),
      se = sd(value)/sqrt(n())
    )

  p1 <- ggplot(athletes_df, aes(x = sex, fill = sex)) + 
    geom_bar() +
    scale_y_continuous(limits = c(0, 95), expand = c(0, 0), name = "number") +
    scale_x_discrete(name = NULL, labels = c("female", "male")) +
    scale_fill_manual(values = c("#D55E00D0", "#0072B2D0"), guide = "none") +
    theme_dviz_hgrid(12, rel_small = 1) + 
    theme(
      #axis.line.x = element_blank(),
      axis.ticks.x = element_blank(),
      #axis.ticks.length = grid::unit(0, "pt"),
      plot.margin = margin(3, 0, 0, 0)
    )

  p2 <- ggplot(cc_df, aes(x = sex, y = mean, fill = sex)) + 
    geom_col() +
    geom_linerange(aes(ymin = mean - se, ymax = mean + se)) +
    facet_wrap(
      ~variable, scales = "free_y",
      labeller = as_labeller(function(label) {
        ifelse(label == "wcc", "WBC count", "RBC count")
      })
    ) +
    scale_y_continuous(limits = c(0, 7.8), expand = c(0, 0), name = "mean count") +
    scale_x_discrete(name = NULL, labels = c("female", "male")) +
    scale_fill_manual(values = c("#D55E00D0", "#0072B2D0"), guide = "none") +
    theme_dviz_hgrid(12, rel_small = 1) +
    theme(
      #axis.line.x = element_blank(),
      axis.ticks.x = element_blank(),
      #axis.ticks.length = grid::unit(0, "pt"),
      plot.margin = margin(3, 0, 0, 0)
    )

  p_row <- plot_grid(
    p1, NULL, p2,
    labels = c("a", "", "b"),
    align = 'h',
    axis = 'l',
    nrow = 1,
    rel_widths = c(0.7, 0.02, 1)
  )

  bf_df <- athletes_df %>%
    select(sex, sport, pcBfat) %>%
    group_by(sex, sport) %>%
    summarize(
      mean = mean(pcBfat),
      se = sd(pcBfat)/sqrt(n())
    )

  p3 <- ggplot(bf_df, aes(x = sport, y = mean, fill = sex)) + 
    geom_col(position = "dodge") +
    geom_linerange(aes(ymin = mean - se, ymax = mean + se), position = position_dodge(width = .9)) +
    scale_y_continuous(limits = c(0, 24.7), expand = c(0, 0), name = "% body fat") +
    scale_x_discrete(name = NULL) +
    scale_fill_manual(
      values = c("#D55E00D0", "#0072B2D0"), name = NULL,
      labels = c("female   ", "male")
    ) +
    guides(fill = guide_legend(
      override.aes = list(color = "white", size = 2),
      direction = "horizontal")
    ) +
    theme_dviz_hgrid(12, rel_small = 1) +
    theme(
      #axis.line.x = element_blank(),
      axis.ticks.x = element_blank(),
      #axis.ticks.length = grid::unit(0, "pt"),
      legend.position = c(1., 0.9),
      legend.justification = "right"
    )

  stamp_ugly(
    plot_grid(
      p_row, NULL, p3,
      ncol = 1,
      rel_heights = c(1, .04, 1),
      labels = c("", "", "c")
    ) +
    theme(plot.margin = margin(6, 6, 3, 1.5))
  )
}
