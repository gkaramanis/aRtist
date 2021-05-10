library(tidyverse)
library(ggforce)

sq <- expand_grid(t = rnorm(1024, 0, 10),
                  s1 = rep(c(-1, 1)),
                  s2 = rep(c(-1, 1))) %>% 
  mutate(
    x = s1 * cos(t),
    y = s2 * sin(t)
  ) %>% 
  rowwise() %>% 
  mutate(
    x2 = list(c(x, cumsum(x + c(s1 * 0.1, rnorm(6, 0, 0.35))))),
    y2 = list(c(y, cumsum(y + c(s2 * 0.1, rnorm(6, 0, 0.35)))))
  ) %>% 
  ungroup() %>% 
  unnest(c(x2, y2))

ggplot(sq) +
  annotate("point", 0, 0, size = 26, color = "grey95") +
  geom_bspline(aes(x2, y2, group = interaction(t, s1, s2), size = ..index.., color = ..index..), lineend = "round", alpha = 0.15) +
  scale_size_continuous(range = c(0.25, 0.4)) +
  scale_color_gradient2(low = "coral1", mid = "purple", high = "blue3", midpoint = 0.55) +
  coord_fixed(expand = FALSE, xlim = c(-10, 10), ylim = c(-10, 10)) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "midnightblue", color = NA)
  ) +
  # ggsave(here::here("iris", "plots", paste0("iris-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, height = 8, width = 8) +
  ggsave(here::here("iris", "plots", "iris.png"), dpi = 320, height = 8, width = 8)

