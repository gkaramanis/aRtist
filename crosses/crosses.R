library(tidyverse)

grid_cross <- expand.grid(
  r = 1:10,
  c = 1:10
  ) %>% 
  mutate(
    x = c + r * 0.333,
    y = r - c * 0.333
  ) %>% 
  rowwise() %>% 
  mutate(
    cx = list(c(x - 0.5, x - 0.165, x - 0.165, x + 0.165, x + 0.165, x + 0.5, x + 0.5, x + 0.5, x + 0.165, x + 0.165, x - 0.165, x - 0.165, x - 0.5)),
    cy = list(c(y - 0.165, y - 0.165, y - 0.5, y - 0.5, y - 0.165, y - 0.165, y + 0.5, y + 0.165, y + 0.165, y + 0.5, y + 0.5, y + 0.165, y + 0.165))
  ) %>% 
  ungroup() %>% 
  unnest(c(cx, cy))


a = pi / 3

anim_cross <- grid_cross %>% 
  mutate(
    cx = x + (cx - x) * cos(a) - (cy - y) * sin(a),
    cy = y + (cx - x) * sin(a) + (cy - y) * cos(a)
  )

ggplot(anim_cross) +
  geom_polygon(aes(cx, cy, group = interaction(r, c), fill = c %% r), color = NA) +
  # geom_polygon(aes(cx + 0.667, cy + 0.333, group = interaction(r, c)), fill = "grey97", color = NA) + # keep this for animation
  scale_fill_viridis_c(option = "magma") +
  coord_fixed(xlim = c(4.5, 9.5), ylim = c(0.5, 5.5)) + # "crop"
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey97", color = NA)
  ) 

ggsave(here::here("crosses", "crosses.png"), dpi = 320, width = 5, height = 5)

