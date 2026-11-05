# from dataviz ch. 29 (telling a story and making a point)
fig_qbio_biorxiv_growth <- function() {
  preprint_growth %>% filter(archive %in% c("bioRxiv", "arXiv q-bio")) %>%
  filter(count > 0) -> df_preprints

  df_final <- filter(df_preprints, date == max(date))
  df_final_qbio <- filter(df_final, archive == "arXiv q-bio")

  ggplot(df_preprints, aes(date, count, color = archive)) + 
    geom_line(size = 0.75) +
    scale_y_log10(
      limits = c(29, 1600),
      breaks = c(30, 100, 300, 1000),
      expand = c(0, 0),
      name = "preprints / month",
      sec.axis = dup_axis(
        breaks = df_final$count,
        labels = df_final$archive,
        name = NULL
      )
    ) + 
    scale_x_date(name = "year", expand = c(0, 0)) +
    scale_color_manual(values = c("#D55E00", "#0072B2"), guide = "none") +
    theme_dviz_grid() +
    theme(
      axis.ticks.y.right = element_blank(),
      axis.text.y.right = element_text(size = 14, margin = margin(0, 0, 0, 0))
    )
}
