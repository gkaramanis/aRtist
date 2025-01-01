library(tidyverse)
library(ambient)
library(camcorder)

gg_record(here::here("genuary/genuary-temp/"), width = 8, height = 8, dpi = 320)

l <- 5
r <- 0.4

circles <- expand_grid(x = 1:l, y = 1:l) %>% 
  mutate(id = cur_group_id(), .by = c(x, y)) %>% 
  crossing(t = seq(0, 4*pi, length.out = 300)) %>% 
  mutate(
    spiral_r = r * t/(2*pi),
    xx = x + spiral_r * cos(t),
    yy = y + spiral_r * sin(t)
  ) %>% 
  filter(between(xx, 1, l) & between(yy, 1, l))

ggplot(circles) +
  geom_segment(aes(x = xx, y = y, yend = yy), linewidth = 1, color = "grey97", alpha = 0.5) +
  coord_fixed() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "grey9", color = NA)
  )