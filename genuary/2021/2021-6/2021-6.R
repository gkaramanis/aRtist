library(tidyverse)
library(ggforce)

del <- data.frame(x = 1:250 * sin(1:50), y = 1:250 * cos(1:50)) %>% 
  mutate(n = row_number())

ggplot(del, aes(x, y)) +
  geom_delaunay_tile(fill = "grey20") +
  geom_delaunay_segment2(aes(color = -n)) +
  scale_color_gradient(low = "grey25", high = "grey98") +
  coord_fixed() +
  xlim(-250, 250) +
  ylim(-250, 250) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey65", color = NA)
  ) 

ggsave(here::here("genuary", "2021", "2021-6", "2021-6.png"), dpi = 320, width = 7, height = 7)

