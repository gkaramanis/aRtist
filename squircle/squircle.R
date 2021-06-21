library(tidyverse)
library(ggforce)

sq <- expand_grid(t = seq(pi/32, pi/2 - pi/32, by = pi/32),
                  s1 = rep(c(-1, 1)),
                  s2 = rep(c(-1, 1))) %>% 
  mutate(
    x = s1 * cos(t)^(1/2),
    y = s2 * sin(t)^(1/2)
  ) %>% 
  rowwise() %>% 
  mutate(
    x2 = list(c(x, cumsum(x + c(s1 * 0.1, rnorm(6, 0, 0.35))))),
    y2 = list(c(y, cumsum(y + c(s2 * 0.1, rnorm(6, 0, 0.35)))))
    ) %>% 
  ungroup() %>% 
  unnest(c(x2, y2))

ggplot(sq) +
  # geom_point(aes(x, y), size = 1.5, color = "firebrick", fill = "orangered4", alpha = 0.5, shape = 21) +
  geom_point(aes(x, y), size = 0.4, color = "grey20") +
  geom_bspline(aes(x2, y2, group = interaction(t, s1, s2), size = ..index..), lineend = "round", color = "grey20") +
  scale_size_continuous(range = c(0.9, 0.25)) +
  scale_color_gradient(low = "orange", high = "orangered") +
  coord_fixed(expand = FALSE, xlim = c(-10, 10), ylim = c(-10, 10)) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey90", color = NA)
  ) 

ggsave(here::here("squircle", "plots", paste0("squircle-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, height = 8, width = 8)
