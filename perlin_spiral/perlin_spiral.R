library(ambient)
library(dplyr)
library(ggplot2)

spiral <- data.frame(a = seq(200, 1000, by = 0.01)) %>% 
  mutate(
    x_off = a * cos(a),
    y_off = a * sin(a),
    r = normalise(gen_perlin(x_off, y_off, frequency = 0.01), from = c(-1, 1), to = c(100, 120)),
    x = a * r * cos(a),
    y = a * r * sin(a)
  ) 
  
ggplot(spiral, aes(x, y)) +
  geom_path(color = "grey10", size = 0.3) +
  coord_fixed() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "grey95", color = NA)
  )

ggsave(here::here("perlin_spiral", "perlin_spiral.png"), dpi = 320, height = 8, width = 8)
