library(tidyverse)
library(ggforce)
library(camcorder)
library(MetBrewer)

gg_record(dir = "genuary/2022/genuary-temp", device = "png", width = 10, height = 10, units = "in", dpi = 320)


trans <- linear_trans(shear(s, 0), rotate(r))
square <- data.frame(x = c(0, 0, 1, 1), y = c(0, 1, 1, 0))

pnts <- expand.grid(
  x0 = round(runif(100, 1, 30)),
  y0 = round(runif(100, 1, 30))
  ) %>% 
  mutate(id = row_number()) %>% 
  rowwise() %>% 
  mutate(
    sq = list(data.frame(x = x0 + c(0, 0, 1, 1), y = y0 + c(0, 1, 1, 0))),
    t = list(trans$transform(sq$x, sq$y, r = id %% 23, s = 0.95))
  ) %>% 
  ungroup() %>% 
  unnest(t)

pal <- met.brewer("Signac", type = "continuous")

ggplot(pnts) +
  geom_polygon(aes(x, y, group = id, fill = id), color = NA) +
  scale_fill_gradientn(colors = pal) +
  coord_fixed() +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey97", color = NA)
  )

