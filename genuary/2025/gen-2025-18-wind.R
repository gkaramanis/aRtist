library(ggplot2)
library(camcorder)

gg_record(here::here("genuary/genuary-temp/"), width = 8, height = 8, dpi = 320)

n <- 150
grid <- expand.grid(x = seq(1, n), y = seq(1, n))

s <- 2
f <- 0.1

grid$x_wind <- grid$x + s * sin(f * grid$y)
grid$y_wind <- grid$y + s * cos(f * grid$x)

grid$d <- sqrt((grid$x_wind - grid$x)^2 + (grid$y_wind - grid$y)^2)

ggplot(grid, aes(x = x_wind, y = y_wind, color = d)) +
  geom_point(aes(size = d)) +
  scale_color_gradient(low = "grey9", high = "grey97") +
  scale_size_continuous(range = c(0.1, 0.8)) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey9")
  ) +
  coord_fixed()
