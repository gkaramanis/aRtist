library(tidyverse)
library(poissoned)
library(ggforce)

pal <- c("#264653", "#2a9d8f", "#e9c46a", "#f4a261", "#e76f51")

pnts <- poisson_disc(50, 50, 1, keep_idx = TRUE)

r <- 2

ggplot(pnts) +
  # geom_point(aes(x, y)) +
  geom_regon(aes(x0 = x, y0 = y, sides = 6, angle = 0, r = r, fill = factor(idx %% 5), color = factor(idx %% 5)), size = 0.4, alpha = 0.5) +
  scale_fill_manual(values = pal) +
  scale_color_manual(values = colorspace::darken(pal, 0.3)) +
  coord_fixed() +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey30", color = NA)
    ) 

ggsave(here::here("genuary", "2021", "2021-23", "2021-23.png"), dpi = 320, width = 7, height = 7)
