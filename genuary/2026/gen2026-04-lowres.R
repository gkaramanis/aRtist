library(tidyverse)
library(camcorder)

gg_record(here::here("genuary/genuary-temp/"), width = 8, height = 8, dpi = 320)

set.seed(99)
i = 10

expand_grid(x = 1:i, y = 1:i) |> 
  mutate(
    angle = atan2(y - i/2, x - i/2),
    radius = sqrt((x - i/2)^2 + (y - i/5)^2),
    spiral_value = radius - angle * 9,
    fill = cut(spiral_value, breaks = seq(-i, i, by = 4), labels = FALSE) %% 2
  ) |> 
  ggplot(aes(x = x, y = y, fill = factor(fill))) +
  geom_tile(color = "white") +
  geom_tile(data = . %>% slice_sample(prop = 0.3), aes(fill = factor(1 - fill)), width = 0.4, height = 0.4) +
  scale_fill_manual(values = c("black", "white"), na.value = "white") +
  coord_fixed(expand = FALSE) +
  theme_void() +
  theme(
    legend.position = "none"
  )

record_polaroid()