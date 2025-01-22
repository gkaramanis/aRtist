library(tidyverse)
library(camcorder)

gg_record(dir = here::here("genuary/genuary-temp/"), width = 8, height = 8, dpi = 320)

tiles <- expand.grid(x = 1:9, y = 1:9, z = 1:25)

ggplot(tiles, aes(x = x, y = y)) +
  geom_tile(aes(fill = factor((y / x) %/% (z/10))), color = "black", linewidth = 1.2) +
  scale_fill_grey(start = 0.15, end = 1) +
  coord_fixed() +
  facet_wrap(vars(z)) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "black", color = NA),
    strip.text = element_blank(),
    panel.spacing = unit(-0.25, "lines")
  )
  
