library(tidyverse)
library(ggforce)
library(poissoned)
library(colorspace)

poi <- poisson_disc(50, 50, xinit = 250, yinit = 250, keep_idx = TRUE) %>% 
  mutate(r = runif(length(x), 2, 8))
  
circ <- poi %>% 
  filter(between(idx, 0, 80) | between(idx, 270, 500))

pnts <- poi %>% 
  filter(between(idx, 80, 270))

ggplot(circ) +
  geom_delaunay_segment(data = pnts, aes(x, y), color ="black", size = 0.75, alpha = 0.9) +
  geom_circle(aes(x0 = x, y0 = y, r = r, size = r), fill = "#a2d5c6", color = "#077b8a", n = 30) +
  geom_circle(aes(x0 = x, y0 = y, r = r/3, size = r/2), fill = "#a2d5c6", color = "#077b8a", n = 30) +
  geom_point(data = pnts, aes(x, y), color = "#077b8a", size = 1) +
  scale_size_continuous(range = c(0.4, 1.8)) +
  coord_fixed(expand = FALSE) +
  xlim(-10, 510) +
  ylim(-10, 510) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#b4a2d5", color = NA)
  ) +
  ggsave(here::here("genuary", "2021", "2021-16", "2021-16.png"), dpi = 320, width = 9, height = 9)

