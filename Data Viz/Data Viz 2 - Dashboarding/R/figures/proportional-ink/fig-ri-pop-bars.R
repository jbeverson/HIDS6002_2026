# from dataviz ch. 17 (proportional ink)
fig_ri_pop_bars <- function() {
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

  ggplot(RI_pop, aes(x = factor(county, levels = rev(county)), y = pop2010, fill = county)) + 
    geom_col() +
    scale_fill_OkabeIto(darken = -.3, guide = "none") +
    scale_y_continuous(
      expand = c(0, 0),
      breaks = c(0, 2e5, 4e5, 6e5),
      labels = c("0", "200,000", "400,000", "600,000"),
      name = "number of inhabitants"
    ) +
    scale_x_discrete(name = "county") +
    coord_flip() +
    theme_dviz_vgrid() +
    theme(
      #axis.line = element_blank(),
      #axis.ticks.length = grid::unit(0, "pt"),
      axis.ticks.y = element_blank(),
      plot.margin = margin(3, 14, 3, 1.5)
    )
}
