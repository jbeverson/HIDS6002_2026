# from dataviz ch. 19 (common pitfalls of color use)
fig_texas_rainbow <- function() {
  library(sf)

  # EPSG:3083
  # NAD83 / Texas Centric Albers Equal Area
  # http://spatialreference.org/ref/epsg/3083/
  texas_crs <- "+proj=aea +lat_1=27.5 +lat_2=35 +lat_0=18 +lon_0=-100 +x_0=1500000 +y_0=6000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

  # -110, -93.5 transformed using texas_crs
  texas_xlim <- c(558298.7, 2112587)

  # we reuse the geometries from texas_income
  texas_race %>% st_sf() %>%
    st_transform(crs = texas_crs) %>%
    filter(variable == "White") %>%
    ggplot(aes(fill = pct)) +
      geom_sf(color = "white") +
      coord_sf(xlim = texas_xlim, datum = NA) + 
      theme_dviz_map() +
      scale_fill_gradientn(
        colors = rainbow(10),
        limits = c(0, 100),
        breaks = 25*(0:4),
        labels = c("0% ", "25%", "50%", "75%", " 100%"),
        name = "percent identifying as white",
        guide = guide_colorbar(
          direction = "horizontal",
          label.position = "bottom",
          title.position = "top",
          ticks = FALSE,
          barwidth = grid::unit(3.0, "in"),
          barheight = grid::unit(0.2, "in")
        )
      ) +
      theme(
        legend.title.align = 0.5,
        legend.text.align = 0.5,
        legend.justification = c(0, 0),
        legend.position = c(0, 0.1)
      ) -> texas_rainbow

  stamp_bad(texas_rainbow)
}
