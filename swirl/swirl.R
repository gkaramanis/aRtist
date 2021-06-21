library(tidyverse)
library(metR)
library(ambient)
library(wesanderson)

r_df <- expand.grid(x = 1:300, y = 1:300)
r_df$dx <- gen_perlin(r_df$x, r_df$y, frequency = 0.15)
r_df$dy <- gen_perlin(r_df$x, r_df$y, frequency = 0.15)

ggplot(r_df, aes(x = x, y = y)) +
  geom_streamline(aes(dx = dx, dy = dy, size = ..step.., alpha = ..step..), arrow = NULL, lineend = "round", L = 20, color = "grey20") +
  scale_size(range = c(0, 0.3), guide = "none") +
  scale_alpha(range = c(0, 0.8), guide = "none") +
  coord_fixed(expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "grey96", color = NA),
    plot.margin = margin(5, 5, 5, 5)
  ) 

ggsave(here::here("swirl", "swirl.png"), dpi = 320, width = 6, height = 6)
