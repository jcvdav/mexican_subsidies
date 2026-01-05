# Bq table versions
vi <- "vessel_info_v_20250815"
vms <- "mex_vms_processed_v_20250623"

# Reset theme
ggplot2::theme_set(ggplot2::theme_bw())

ggplot2::theme_update(
  text = ggplot2::element_text(color = "black"),
  axis.text = ggplot2::element_text(color = "black"),
  axis.title.y = ggplot2::element_text(size = 10),
  axis.title.x = ggplot2::element_text(size = 10),
  axis.text.y = ggplot2::element_text(size = 8),
  axis.text.x = ggplot2::element_text(size = 8),
  panel.background = ggplot2::element_blank(),
  plot.background = ggplot2::element_blank(),
  legend.background = ggplot2::element_blank(),
  legend.key = ggplot2::element_blank(),
  panel.grid.major.x = ggplot2::element_blank(),#element_line(colour = "gray", linewidth = 0.1),
  panel.grid.major.y = ggplot2::element_line(colour = "gray",
                                    linewidth = 0.1,
                                    linetype = "dashed"),
  panel.grid.minor = ggplot2::element_blank(),
  strip.background = ggplot2::element_blank()
)

ggplot2::update_geom_defaults(geom = "point",
                              new = list(color = "black",
                                         fill = "steelblue",
                                         shape = 21,
                                         size = 2))

ggplot2::update_geom_defaults(geom = "col",
                              new = list(color = "black",
                                         fill = "steelblue"))

ggplot2::update_geom_defaults(geom = "bar",
                              new = list(color = "black",
                                         fill = "steelblue"))

ggplot2::update_geom_defaults(geom = "area",
                              new = list(color = "black",
                                         fill = "steelblue"))

ggplot2::update_geom_defaults(geom = "segment",
                              new = list(color = "black",
                                         linetype = "dashed"))

ggplot2::update_geom_defaults(geom = "hline",
                              new = list(color = "black",
                                         linetype = "dashed"))
