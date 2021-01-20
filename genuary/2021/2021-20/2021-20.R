library(tidyverse)
library(ggbump)
library(wesanderson)

centroids <- read_tsv(here::here("genuary", "2021", "2021-20", "data", "country-centroids.csv"))

pal <- wes_palette("Darjeeling1", 244, type = "continuous")

ggplot(centroids) +
  geom_point(aes(longitude, latitude, color = factor(longitude/latitude)), size = 1.5, shape = 15) +
  geom_sigmoid(aes(x = longitude/5, y = -150, xend = longitude, yend = latitude, group = country, color = factor(longitude/latitude)), direction = "y", size = 0.2, alpha = 0.7) +
  geom_sigmoid(aes(x = longitude/5, y = 150, xend = longitude, yend = latitude, group = country, color = factor(longitude/latitude)), direction = "y", size = 0.2, alpha = 0.7) +
  geom_sigmoid(aes(x = -250, y = latitude/4, xend = longitude, yend = latitude, group = country, color = factor(longitude/latitude)), direction = "x", size = 0.2, alpha = 0.7) +
  geom_sigmoid(aes(x = 250, y = latitude/4, xend = longitude, yend = latitude, group = country, color = factor(longitude/latitude)), direction = "x", size = 0.2, alpha = 0.7) +
  # geom_text(aes(longitude, latitude, label = country)) +
  scale_color_manual(values = pal) +
  coord_fixed(expand = FALSE) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey25", color = NA)
  ) +
  ggsave(here::here("genuary", "2021", "2021-20", "2021-20.png"), dpi = 320, height = 5.4, width = 9)
