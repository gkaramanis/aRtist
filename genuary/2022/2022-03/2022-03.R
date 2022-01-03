library(ggshadow)
library(ggplot2)
library(camcorder)

gg_record(dir = "genuary/2022/genuary-temp", device = "png", width = 10, height = 10, units = "in", dpi = 320)

pal <- MetBrewer::met.brewer("Hiroshige")

set.seed(2022)

n = 80

stars <- data.frame(
  x = runif(n * 2),
  y = runif(n * 2),
  size = runif(n * 2, 0, 0.2),
  x2 = runif(n),
  y2 = runif(n),
  size2 = runif(n, 0, 3),
  color2 = pal[runif(n, 5, 10)]
)

ggplot(stars) +
  geom_point(aes(x, y, size = size), color = "grey67") +
  geom_glowpoint(aes(x2, y2, size = size2, color = color2, shadowsize = size2 ^ 1.2)) +
  scale_size_continuous(range = c(0.1, 4)) +
  scale_color_identity() +
  coord_fixed(clip = "off") +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey10", color = NA)
  )
  