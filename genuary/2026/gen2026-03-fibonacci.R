library(tidyverse)
library(camcorder)

gg_record(here::here("genuary/genuary-temp/"), width = 8, height = 8, dpi = 320)

set.seed(99)
i = 80

fib_seq <- c(1, 1)

while (tail(fib_seq, 1) < i) {
  fib_seq <- c(fib_seq, fib_seq[length(fib_seq)] + fib_seq[length(fib_seq) - 1])
}

fib_seq <- unique(fib_seq[fib_seq <= i])

breaks <- unique(c(-i, fib_seq, i))

expand_grid(x = 1:i, y = 1:i) |> 
  mutate(
    angle = atan2(y - i/3, x - i/6),
    radius = sqrt((x - i/2)^2 + (y - i/2)^2),
    spiral_value = radius - angle * 9,
    fill = cut(spiral_value, breaks = breaks, labels = FALSE) %% 5
  ) |> 
  ggplot(aes(x = x, y = y, fill = factor(fill))) +
  geom_tile(color = "white") +
  geom_tile(data = . %>% slice_sample(prop = 0.3), aes(fill = factor(3 - fill)), width = 0.4, height = 0.4) +
  MetBrewer::scale_fill_met_d("Johnson") +
  coord_fixed(expand = FALSE) +
  theme_void() +
  theme(
    legend.position = "none"
  )

record_polaroid()
