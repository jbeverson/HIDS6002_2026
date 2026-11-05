# from dataviz ch. 17 (proportional ink)
fig_ri_pop_pie <- function() {
  RI_pop <- US_census %>% 
    filter(state == "Rhode Island") %>%
    extract(name, "county", regex = "(.+) County") %>%
    select(county, pop2010) %>%
    mutate(
      label_pop = scales::comma(signif(pop2010, 3)),
      label_comb = paste(county, label_pop, sep = "\n")
    ) %>%
    arrange(desc(pop2010))

  RI_pop$county = factor(RI_pop$county, levels = RI_pop$county)

  RI_pie <- RI_pop %>%
    mutate(
      total = sum(pop2010),
      end_angle = 2*pi*cumsum(pop2010)/total,      # ending angle for each pie slice
      start_angle = lag(end_angle, default = 0),   # starting angle for each pie slice
      mid_angle = 0.5*(start_angle + end_angle),   # middle of each pie slice, for the text label
      hjust = ifelse(mid_angle>pi, 1, 0),
      vjust = ifelse(mid_angle<pi/2 | mid_angle>3*pi/2, 0, 1)
    )

  rpie = 1

  RI_pie$rlabel = c(.3, .4, .4, 1.02, 1.02) * rpie
  RI_pie$size = c(16, 14, 14, 12, 12)/.pt

  plot_RI_pies <- ggplot(RI_pie) + 
    geom_arc_bar(
      aes(
        x0 = 0, y0 = 0, r0 = 0, r = rpie,
        start = start_angle, end = end_angle, fill = county
      ),
      color = "white", size = 0.75
    ) +
    geom_text(
      aes(
        x = rlabel*sin(mid_angle),
        y = rlabel*cos(mid_angle),
        label = label_pop,
        hjust = hjust, vjust = vjust,
        size = size
      ),
      family = dviz_font_family
    ) +
    coord_fixed() +
    scale_x_continuous(limits = c(-1.1, 1.1), expand = c(0, 0), name = NULL, breaks = NULL, labels = NULL) +
    scale_y_continuous(limits = c(-1.1, 1.1), expand = c(0, 0),name = NULL, breaks = NULL, labels = NULL) +
    #scale_fill_brewer(type = "qual", palette = "Pastel1") +
    scale_fill_OkabeIto(darken = -.3) +
    scale_size_identity() +
    guides(fill = guide_legend(override.aes = list(size = 1.))) +
    theme_dviz_open() +
    theme(
      axis.line.x = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks.y = element_blank(),
      legend.title.align = 0.5,
      legend.key.size = grid::unit(25, "pt"),
      legend.spacing.x = grid::unit(2, "pt"),
      legend.spacing.y = grid::unit(2, "pt"),
      plot.margin = margin(7, 7, 3, 1.5)
    )

  plot_RI_pies
}
