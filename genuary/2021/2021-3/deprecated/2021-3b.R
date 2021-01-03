library(tidyverse)

n = 30

faces <- data.frame(x = 1:n, y = 1:n) %>% 
  expand(x, y) %>% 
  mutate(f = row_number()) %>% 
  rowwise() %>% 
  mutate(
    mouth_x = list(c(x - 0.2, x, x + 0.2)),
    mouth_y = list(-0.15 + c(y, y + sample(-1:1, 1) / 10, y))
    ) %>% 
  ungroup()

faces %>% 
  unnest(c(mouth_x, mouth_y)) %>% 
  ggplot() +
  # face
  # geom_circle(aes(x0 = x, y0 = y, r = 0.45), fill = "yellow", color = NA, n = 10) +
  # mouth
  geom_path(aes(mouth_x, mouth_y, group = f), size = 0.4) +
  # eyes
  geom_point(aes(x = x - 0.2, y = y + 0.1), size = 0.4) +
  geom_point(aes(x = x + 0.2, y = y + 0.1), size = 0.4) +
  coord_fixed() +
  theme_void() +
  ggsave(here::here("genuary", "2021", "2021-3b.png"), dpi = 320, width = 7, height = 3.7)

