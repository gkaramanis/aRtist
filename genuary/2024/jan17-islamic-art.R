library(tidyverse)
library(ggstar)
library(camcorder)

gg_record(here::here("genuary/genuary-temp/"), width = 8, height = 8, dpi = 320)

expand_grid(x = 0:11, y = 0:11) %>%
  ggplot() +
  geom_star(aes(x, y), color = "white", fill = NA, size = 24, starshape = 8, angle = 22.5) +
  geom_star(aes(x, y), color = "black", fill = "white", size = 25.5, starshape = 29, alpha = 0.6) +
  geom_star(aes(x, y), color = "black", fill = "white", size = 25.5, starshape = 29, angle = 45, alpha = 0.6) +
  geom_star(aes(x + 0.5, y + 0.5), color = "white", fill = "grey80", size = 6, starshape = 4) +
  geom_star(aes(x + 0.5, y + 0.5), color = "white", fill = NA, size = 14, starshape = 4) +
  geom_star(aes(x + 0.5, y + 0.5), color = "black", fill = NA, size = 16, starshape = 4) +
  coord_fixed(xlim = c(1, 10), ylim = c(1, 10)) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey15", color = NA)
  )
