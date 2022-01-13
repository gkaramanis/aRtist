library(ggforce)
library(camcorder)
library(MetBrewer)

gg_record(dir = "genuary/2022/genuary-temp", device = "png", width = 2.5, height = 0.25, units = "in", dpi = 320) # 800x80 pixels (dimension / dpi)

n = 1000

plot_f <- function(s) {
  ggplot() +
  geom_voronoi_tile(aes(x = seq(0, 800, length.out = n), y = 40 + 40 * sin(1:n / s), fill = 1:n %% 12), color = "grey97", size = 0.2) +
  scale_fill_gradientn(colours = met.brewer("Hiroshige")) +
  coord_fixed(xlim = c(0, 800),  ylim = c(0, 80), expand = FALSE) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey97", color = "grey20")
  )
  }

plot_f(2)
plot_f(4)
plot_f(5)
plot_f(6)
plot_f(8)
