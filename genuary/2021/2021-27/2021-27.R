library(tidyverse)

xs <- data.frame(
  x = 1:100,
  y = 1:100) %>% 
  expand(x, y)

ggplot(xs) +
  geom_point(aes(x, y, size = y %/% 20)) +
  geom_point(aes(y, x, size = y %/% 20)) +
  scale_size_continuous(range = c(0.5, 1.8)) +
  scale_y_reverse() +
  coord_fixed() +
  theme_void() +
  theme(
    legend.position = "none"
  ) 

ggsave(here::here("genuary", "2021", "2021-27", "2021-27.png"), dpi = 320, width = 7, height = 7)

