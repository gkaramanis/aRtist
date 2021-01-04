library(tidyverse)
library(wesanderson)

n = 25
pal <- wes_palette("FantasticFox1", 700, type = "continuous")

grd <- data.frame(x = 1:n, y = 1:n) %>% 
  expand(x, y) %>% 
  mutate(r = row_number()) %>% 
  rowwise() %>% 
  mutate(
    cx = list(c(abs(cos(2 * seq(0, 2*pi, by = 0.126))) * cos(seq(0, 2*pi, by = 0.126)))),
    cy = list(c(abs(cos(2 * seq(0, 2*pi, by = 0.126))) * sin(seq(0, 2*pi, by = 0.126)))),
  ) %>% 
  ungroup() %>% 
  unnest(c(cx, cy))

ggplot(grd) +
  geom_polygon(aes(x + 0.8 * cx, y + 0.8 * cy, group = r, fill = interaction(x-y, y-x)), color = NA) +
  geom_polygon(aes(x + 0.25 * cx, y + 0.25 * cy, group = r, fill = interaction(y, x)), color = NA) +
  geom_polygon(aes(0.5 + x + 0.45 * cx, 0.5 + y + 0.45 * cy, group = r, fill = interaction(x, y)), color = NA) +
  scale_fill_manual(values = pal) +
  coord_fixed(expand = FALSE) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey90", color = NA),
    plot.margin = margin(10, 10, 10, 10)
    ) +
		ggsave(here::here("genuary", "2021", "2021-4", "2021-4.png"), dpi = 320, width = 7, height = 7)
