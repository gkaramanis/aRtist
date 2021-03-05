library(tidyverse)
library(ambient)

v_lines <- expand.grid(r = 1:8, c = 1:8) %>% 
  mutate(n = row_number()) %>% 
  rowwise() %>% 
  mutate(
    x = list(1:4),
    j = runif(1, 0, 0.2)
  ) %>% 
  ungroup() %>% 
  unnest(x) %>% 
  mutate(
    c = c + j,
    r = r - j
    ) %>% 
  rowwise() %>% 
  mutate(s = runif(1, 0.8, 1.5)) %>% 
  ungroup() %>% 
  filter(n != max(n))

h_lines <- expand.grid(r = 1:8, c = 1:8) %>% 
  mutate(n = row_number()) %>% 
  rowwise() %>% 
  mutate(s = runif(1, 0.8, 1.5)) %>% 
  ungroup() %>% 
  filter(n != max(n))

ggplot() +
  geom_segment(data = v_lines, aes(x = 10 * c + x, y = r * 12,
                   xend = 10 * c + x, yend = r * 12 + 7,
                   group = n, size = s), position = position_jitter(width = 0.25, height = 0.4), lineend = "round") +
  geom_segment(data = h_lines, aes(x = 10 * c + 0.5, y = r * 12 + 2,
      xend = 10 * c + 5.5, yend = r * 12 + 3, size = s),
  position = position_jitter(width = 0.5, height = 1),
  stat = "unique", lineend = "round") +
  scale_y_reverse() +
  scale_size_identity() +
  coord_fixed() +
  labs(title = "315 days") +
  theme_void() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 30, family = "Humor Sans", hjust  = 0.5),
    plot.margin = margin(20, 0, 0, 0)
    ) +
  ggsave(here::here("tally marks", "tally-marks.png"), dpi = 320, height = 8, width = 6.5)

