library(tidyverse)
library(ggbump)
library(wesanderson)

s = 1
e = 5
f = 0.7

lns <- data.frame(
  a <- seq(0, 2*pi, length.out = 500)
  ) %>% 
  mutate(
    x = s * cos(a + f),
    y = s * sin(a + f),
    xend = e * cos(a),
    yend = e * sin(a)
  )

pal <- wes_palette("FantasticFox1", 500, type = "continuous")

ggplot(lns) +
  geom_sigmoid(aes(x = x, y = y, xend = xend, yend = yend, group = a, color = factor(y ^ 4)), size = 0.4, smooth = 5) +
  # scale_color_viridis_c() +
  scale_color_manual(values = pal) +
  coord_fixed() +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey20", color = NA)
  ) 

ggsave(here::here("genuary", "2021", "2021-24", "2021-24.png"), dpi = 320, width = 7, height = 7)

