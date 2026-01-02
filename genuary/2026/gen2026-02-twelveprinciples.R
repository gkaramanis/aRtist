library(tidyverse)
library(camcorder)
library(gganimate)

gg_record(here::here("genuary/genuary-temp/"), width = 8, height = 8, dpi = 320)

set.seed(99)
i = 80
n_frames = 60

expand_grid(x = 1:i, y = 1:i, frame = 1:n_frames) |> 
  mutate(
    angle = atan2(y - i/2, x - i/2),
    radius = sqrt((x - i/2)^2 + (y - i/5)^2),
    phase = (frame - 1) * (2 * pi / n_frames),
    spiral_value = radius - (angle + phase) * 9,
    fill = cut(spiral_value, breaks = seq(-i, i, by = 4), labels = FALSE) %% 2
  ) |> 
  ggplot(aes(x = x, y = y, fill = factor(fill))) +
  geom_tile(color = "white") +
  geom_tile(data = . %>% slice_sample(prop = 0.3), aes(fill = factor(1 - fill)), width = 0.4, height = 0.4) +
  scale_fill_manual(values = c("black", "white"), na.value = "white") +
  coord_fixed(expand = FALSE) +
  theme_void() +
  theme(legend.position = "none") +
  transition_manual(frame) 

anim_save(here::here("genuary/2026/gen2026-02-twelveprinciples.gif"), animation = last_animation(), fps = 10, width = 800, height = 800)
