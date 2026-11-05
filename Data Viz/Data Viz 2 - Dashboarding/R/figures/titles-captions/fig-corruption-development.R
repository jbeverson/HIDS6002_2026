# from dataviz ch. 22 (titles, captions, and tables)
fig_corruption_development <- function() {
  country_highlight <- c("Germany", "Norway", "United States", "Greece", "Singapore", "Rwanda", "Russia", "Venezuela", "Sudan", "Iraq", "Ghana", "Niger", "Chad", "Kuwait", "Qatar", "Myanmar", "Nepal", "Chile", "Argentina", "Japan", "China")


  corruption %>% filter(year == 2015) %>% na.omit() %>%
    mutate(region = case_when(
      region == "Middle East and North Africa" ~ "Middle East\nand North Africa",
      region == "Europe and Central Asia" ~ "Europe and\nCentral Asia",
      region == "Sub Saharan Africa" ~ "Sub-Saharan\nAfrica",
      TRUE ~ region),
      label = ifelse(country %in% country_highlight, country, "")
      ) %>%
    ggplot(aes(cpi, hdi)) + 
      geom_smooth(
        aes(color = "y ~ log(x)", fill = "y ~ log(x)"),
        method = 'lm', formula = y~log(x), se = FALSE, fullrange = TRUE
      ) +
      geom_point(
        aes(color = region, fill = region),
        size = 2.5, alpha = 0.5, shape = 21
      ) + 
      geom_text_repel(
        aes(label = label), color = "black", size = 10/.pt,
        point.padding = 0.1, box.padding = .6, force = 1.,
        min.segment.length = 0, seed = 7654,
        family = dviz_font_family
      ) +
      scale_color_OkabeIto(name = NULL, order = c(1:5, 8), darken = 0.3) +
      scale_fill_OkabeIto(name = NULL, order = c(1:5, 8)) +
      scale_y_continuous(
        limits = c(0.3, 1.05), breaks = c(0.2, 0.4, 0.6, 0.8, 1.0),
        expand = c(0, 0),
        name = "Human Development Index, 2015\n(1.0 = most developed)"
      ) +
      scale_x_continuous(
        limits = c(10, 95),
        breaks = c(20, 40, 60, 80, 100),
        expand = c(0, 0),
        name = "Corruption Perceptions Index, 2015 (100 = least corrupt)"
      ) +
      guides(
        color = guide_legend(
          nrow = 1,
          override.aes = list(
            linetype = c(rep("blank", 5), "solid"),
            shape = c(rep(21, 5), NA)
          )
        )
      ) +
      theme_dviz_hgrid(12, rel_small = 1) +
      theme(
        legend.position = "top",
        legend.justification = "right",
        legend.text = element_text(size = 10),
        legend.box.spacing = unit(0, "pt")
      ) -> plot_corrupt_base

  plot_corrupt_base
}
