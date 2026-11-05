# from dataviz ch. 23 (balance the data and the context)
fig_titanic_facets_bad <- function() {
  titanic %>% mutate(surv = ifelse(survived == 0, "died", "survived")) %>%
    ggplot(aes(sex, fill = sex)) + geom_bar() +
      facet_grid(class ~ surv, scales = "free_x") +
      scale_x_discrete(name = NULL) + 
      scale_y_continuous(limits = c(0, 195), expand = c(0, 0)) +
      scale_fill_manual(values = c("#D55E00D0", "#0072B2D0"), guide = "none") +
      theme_dviz_open(rel_small = 1) +
      theme(axis.line.x = element_blank(),
            axis.ticks.x = element_blank(),
            #axis.text.x = element_text(margin = margin(7, 0, 0, 0)),
            strip.text = element_text(margin = margin(3.5, 3.5, 3.5, 3.5)),
            strip.background  = element_blank(),
            plot.margin = margin(3.5, 14, 3.5, 1.5)) -> p

  stamp_ugly(p)
}
