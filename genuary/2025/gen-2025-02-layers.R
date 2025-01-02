library(tidyverse)
library(ggforce)
library(camcorder)

gg_record(here::here("genuary/genuary-temp/"), width = 8, height = 8, dpi = 320)

set.seed(99)

n <- 600

centers <- tibble(
  id = 1:n,
  dist_from_center = sort(runif(n, 0, 50)), 
  angle = runif(n, 0, 2*pi)
) %>%
  mutate(
    x = 50 + dist_from_center * cos(angle),
    y = 50 + dist_from_center * sin(angle)
  )

satellites <- centers %>%
  mutate(n_sats = sample(4:9, n, replace = TRUE)) %>% 
  uncount(n_sats) %>%
  group_by(id) %>%
  mutate(
    angle = runif(n(), 0, 2*pi),
    dist = runif(n(),  dist_from_center/8, dist_from_center/8 * 1.5),
    x = x + dist * cos(angle),
    y = y + dist * sin(angle)
  ) %>%
  arrange(angle) %>% 
  select(id, x, y)

ggplot(satellites, aes(x, y, group = id)) +
  geom_bspline_closed(alpha = 0.3, fill = "grey95", color = "grey40", linewidth = 0.1) +
  coord_fixed() +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey9", color = NA)
  )
