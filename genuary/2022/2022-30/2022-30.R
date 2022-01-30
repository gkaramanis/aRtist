library(tidyverse)
library(camcorder)
library(ggforce)
library(MetBrewer)

gg_record(dir = "genuary/2022/genuary-temp", device = "png", width = 10, height = 10, units = "in", dpi = 320)

k = 7
p = 10
s = 0.015

spiral <- data.frame(t = seq(0, pi * p, s)) %>% 
  mutate(
    x = exp(t/k) * cos(t),
    y = exp(t/k) * sin(t),
    i = row_number()
  )

pal <- met.brewer("Hiroshige")

ggplot(spiral) +
  geom_regon(aes(x0 = x, y0 = y, sides = 4, r = 1.22^t, angle = t, size = i^10, color = i %% 20), fill = NA) +
  scale_size_continuous(range = c(0, 0.7)) +
  scale_color_gradientn(colors = pal) +
  coord_fixed() +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey97", color = NA)
  )

