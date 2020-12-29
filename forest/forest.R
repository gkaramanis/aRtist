library(tidyverse)
library(ggforce)
library(colorspace)

trees <- data.frame(
  x = runif(40, 0, 100),
  size = runif(40, 0.5, 2)
) %>% 
  arrange(size)

leaves <- data.frame(
  x = runif(20, 0, 100),
  size = runif(20, 20, 80)
)

leaves_bg <- data.frame(
  x = runif(20, 0, 100),
  size = runif(20, 40, 80)
)

ggplot() +
  geom_point(data = leaves_bg, aes(x = x, y = 10, size = size), color = "#447081") +
  scale_size_identity() +
  geom_tile(data = trees, aes(x = x, y = 25, width = size, height = 50, alpha = size), fill = "#695B4D", color = NA) +
  geom_point(data = leaves, aes(x = x, y = 0, size = size), color = "#45593A") +
  geom_point(data = leaves, aes(x = 100-x, y = 50, size = size), color = "#45593A") +
  scale_size_identity() +
  scale_alpha_continuous(range = c(0.5, 1)) +
  coord_fixed(expand = FALSE) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#A3B7D2", color = NA)
    ) +
  xlim(0, 100) +
  ylim(0, 50)
