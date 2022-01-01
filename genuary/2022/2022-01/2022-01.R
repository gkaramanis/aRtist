library(tidyverse)
library(ggforce)
library(ambient)
library(MetBrewer)
library(camcorder)

gg_record(dir = "genuary/2022/genuary-temp", device = "png", width = 10, height = 10, units = "in", dpi = 320)

set.seed(20)

tenk <- long_grid(seq(1, 10, length.out = 100), seq(1, 10, length.out = 100))
tenk$d <- gen_perlin(tenk$x, tenk$y, frequency = 0.25)

ggplot(tenk) +
  geom_regon(aes(x0 = x, y0 = y, r = 2.5, fill = d, sides = 3, angle = d * 4.5), size = 0.1, color = NA, alpha = 0.1) +
  scale_fill_gradientn(colors = rev(met.brewer("VanGogh2"))) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey97", color = NA)
  )
