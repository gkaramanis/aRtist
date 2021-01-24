# https://bookdown.org/rdpeng/RProgDA/building-new-graphical-elements.html

library(grid)

GeomMyPoint <- ggproto("GeomMyPoint", Geom,
                       required_aes = c("x", "y"),
                       default_aes = aes(shape = 1),
                       draw_key = draw_key_point,
                       draw_panel = function(data, panel_scales, coord) {
                         ## Transform the data first
                         coords <- coord$transform(data, panel_scales)
                         
                         ## Construct a grid grob
                         polygonGrob(
                           x = coords$x,
                           y = coords$y
                         )
                       }) 


geom_mypoint <- function(mapping = NULL, data = NULL, stat = "identity",
                         position = "identity", na.rm = FALSE, 
                         show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomMyPoint, mapping = mapping,  
    data = data, stat = stat, position = position, 
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

ggplot(data = cube_df, aes(x, y, group = zorder)) +
  geom_mypoint(fill = "pink") +
  coord_fixed()
