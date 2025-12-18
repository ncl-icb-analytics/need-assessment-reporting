

# Define your custom theme

theme_nwl <- function(base_size = 12, base_family = "sans") {
  ggplot2::theme_bw(base_size, base_family) +
    ggplot2::theme(
      panel.spacing = ggplot2::unit(1.5, "lines"),
      panel.border = ggplot2::element_rect(color = "grey50", fill = NA, size = 1, linetype = 1),
      plot.background = ggplot2::element_rect(fill = "white", colour = "white"),
      panel.background = ggplot2::element_rect(fill = "white", colour = "white"),
      panel.grid=ggplot2::element_line(colour = "grey70"),
      strip.text = ggplot2::element_text(colour = "white", face = "bold"),
      axis.title = ggplot2::element_text(colour = "grey50", face = "bold"),
      strip.background = ggplot2::element_rect(colour = "grey50", fill = "grey50"),
      legend.background = ggplot2::element_blank(),
      legend.title = ggplot2::element_text(colour = "grey50", face = "bold"),
      panel.grid.major = ggplot2::element_line(size = 0.3),
      panel.grid.minor = element_line(linetype = 3, size=0.3), 
      axis.ticks = element_line(size = 0.5),
      plot.caption = ggplot2::element_text(size = 11, lineheight = 0.5, color="grey50", hjust=0),
      plot.title.position = "panel"
      #plot.caption.position = "plot"
      #plot.title = ggplot2::element_text(margin = margin(5,5,5,5), hjust = 0.11),
      #plot.title.position = "plot"
    )
}
