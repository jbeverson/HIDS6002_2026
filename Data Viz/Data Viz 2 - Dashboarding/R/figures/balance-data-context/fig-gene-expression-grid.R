# from dataviz ch. 23 (balance the data and the context)
fig_gene_expression_grid <- function() {
  gene_expression %>% 
    filter(strain == "phi9v2" & background == "wt") %>%
    ggplot(aes(x=tpm_wt, y=tpm_mutant)) +
    geom_point(pch = 21, fill = "#0072B2D0", color = "white", size = 3) +
    geom_text_repel(
      aes(label = label),
      nudge_y = -0.005,
      fontface = "italic",
      family = "sans",
      box.padding = 0.35,
      size = 12/.pt,
      segment.color = NA) +
    scale_x_log10(
      limits = c(2e-4, 3.3e-1),
      name = "wild-type mRNA abundance (TPM)",
      breaks = c(3.2e-4, 1e-3, 3.2e-3, 1e-2, 3.2e-2, 1e-1, 3.2e-1),
      labels = expression("", 10^-3, "", 10^-2, "", 10^-1, "")
    ) + 
    scale_y_log10(
      limits = c(2e-4, 3.3e-1),
      name = "mutant mRNA abundance (TPM)",
      breaks = c(3.2e-4, 1e-3, 3.2e-3, 1e-2, 3.2e-2, 1e-1, 3.2e-1),
      labels = expression("", 10^-3, "", 10^-2, "", 10^-1, "")
    ) + 
    coord_fixed(expand = FALSE, clip = "off") +
    theme_dviz_open() +
    theme(
      panel.grid.major = element_line(colour = "grey80", size = 0.5, linetype = 1),
      plot.margin = margin(17.5, 3.5, 3.5, 1.5)
    ) -> gene_expression_plot_bad


  stamp_bad(gene_expression_plot_bad)
}
