library(packcircles)
library(scales)
library(tidyverse)
library(camcorder)
library(ggforce)

gg_record(dir = "genuary/2022/genuary-temp", device = "png", width = 10, height = 10, units = "in", dpi = 320)

# Original code by Chi
# https://chichacha.netlify.app/2018/12/22/bubble-packed-chart-with-r-using-packcircles-package/

set.seed(1999)

pack_layout <- circleProgressiveLayout(abs(rnorm(150, 120, 50)), sizetype = "radius") %>% 
  mutate(id = row_number())

data_gg <- circleLayoutVertices(pack_layout) %>% 
  inner_join(pack_layout %>% select(-x, -y), by = "id")


ggplot(data_gg) + 
  geom_circle(aes(x0 = x, y0 = y, r = radius/3, group = id), n = 50, color = "#2A427745") +
  coord_fixed() +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#F2EBDD", color = NA)
  )

