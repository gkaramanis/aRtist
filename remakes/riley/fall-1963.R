library(tidyverse)

set.seed(1963)

fall <- data.frame(n = 0:105) %>% 
  rowwise() %>% 
  mutate(
    x = list(seq(2, 45, 0.1)),
    s = runif(1, 0.35, 0.55)
    ) %>% 
  ungroup() %>% 
  unnest(x) %>% 
  mutate(
    y = n + 2.2 * sin(6 * pi * x^(1/4)),
    s = abs(y - n)/10 + s
    )

ggplot(fall) +
  geom_line(aes(x = x, y = y, group = n, size = s), lineend = "round") +
  # scale_size_continuous(range = c(0.4, 0.8)) +
  scale_size_identity() +
  coord_flip(clip = "off") +
  ylim(5, 100) +
  theme_void() +
  theme(
    legend.position = "none"
  ) +
  ggsave(here::here("remakes", "riley", "fall-1963.png"), dpi = 320, width = 5, height = 5)

