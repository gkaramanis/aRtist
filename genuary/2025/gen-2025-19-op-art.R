library(tidyverse)
library(ggforce)
library(camcorder)

gg_record(here::here("genuary/genuary-temp/"), width = 8, height = 8, dpi = 320)

n <- 50

grid_points <- expand.grid(
  x = seq(-0.2, 1.2, length.out = n),
  y = seq(-0.2, 1.2, length.out = n)
  ) %>% 
  group_by(y) %>% 
  mutate(
    i = cur_group_id(),
    iy = y,
    x = x + 0.03 * sin(9 * pi * y),
    y = y + 0.01 * cos(9 * pi * x),
    ) %>% 
  ungroup()

ggplot(grid_points, aes(x = x, y = y, group = i, color = y-iy < 0.002, fill = after_scale(rev(color)))) +
  geom_tile(height = 0.018, width = 0.018, linewidth = 1) +
  scale_color_manual(values = c("grey9", "grey97")) +
  scale_fill_manual(values = c("grey9", "grey97")) +
  coord_fixed(clip = "off", xlim = c(0, 1), ylim = c(0, 1), expand  = FALSE) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey9")
    )