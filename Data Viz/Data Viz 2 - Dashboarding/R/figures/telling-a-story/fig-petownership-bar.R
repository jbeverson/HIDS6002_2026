# from dataviz ch. 29 (telling a story and making a point)
fig_petownership_bar <- function() {
  # source: 2012 U.S. Pet Ownership & Demographics Sourcebook
  # American Veterinary Medical Association
  # https://www.avma.org/KB/Resources/Statistics/Pages/Market-research-statistics-US-pet-ownership.aspx

  df <- read.table(text = "pet  households
  dogs 43346000
  cats 36117000
  fish	7738000
  birds 3671000
  horses 1780000
  rabbits	1408000
  turtles	1320000
  poultry	1020000
  ", header = TRUE)


  df$pet <- factor(df$pet, levels = rev(df$pet))
  df <- filter(df, households > 3500000) %>%
    mutate(highlight = ifelse(pet == "cats", "yes", "no"))

  ggplot(df, aes(x = pet, y = households, fill = highlight)) +
    geom_col() +
    geom_label(
      aes(label = paste0(signif(households*1e-6, 2), "M")),
      hjust = 0,
      nudge_y = 0.1e6,
      size = 14/.pt,
      family = dviz_font_family,
      label.size = 0, # no label outline
      label.padding = unit(2, "pt"),
      fill = "#FFFFFF"
    ) +
    coord_flip(clip = "off") +
    scale_x_discrete(
      name = NULL
    ) +
    scale_y_continuous(
      limits = 1e6*c(0, 47),
      breaks = 1e7*(0:4),
      labels = c("0", paste0(10*(1:4), "M")),
      name = "households",
      #position = "right",
      expand = c(0, 0)
    ) +
    scale_fill_manual(values = c("#B0B0B0D0", "#BD3828D0"), guide = "none") +
    theme_dviz_vgrid(rel_small = 1) +
    theme(
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.ticks.length = grid::unit(0, "pt")
    )
}
