library(tidyverse)
library(ggforce)
library(camcorder)

gg_record(dir = here::here("asemic", "temp"), dpi = 320, width = 8, height = 6)

np = 15
s = 8
z = 0.7

flo <- expand_grid(x = 1:(1.5 * np), y = 1:np) %>% 
  rowwise() %>% 
  mutate(
    a = list(runif(s, 0, 2 * pi)),
    rx = list(x + seq(0.1, 0.5, length.out = s) * cos(a)),
    ry = list(y + seq(0.1, 0.3, length.out = s) * sin(a))
    ) %>% 
  ungroup() %>% 
  unnest(c(rx, ry)) %>% 
  rowwise() %>% 
  mutate(
    size = runif(1, 0, 2),
    size = if_else(size < z, 0, size)
    ) %>% 
  ungroup()

ggplot(flo, aes(x = rx, y = ry, group = y, size = size)) +
  # geom_point(color = "pink") +
  geom_bspline2(n = 800, lineend = "round") +
  scale_size_continuous(range = c(0, 0.8)) +
  coord_fixed() +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey97", color = NA)
  )

