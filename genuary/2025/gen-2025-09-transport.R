library(ggplot2)
library(data.table)

grid_size <- 50
rotation <- pi/6

x <- rep(1:grid_size, each = grid_size)
y <- rep(1:grid_size, times = grid_size)

set.seed(999)
offset_x <- rnorm(length(x), sd = 0.2)
offset_y <- rnorm(length(y), sd = 0.2)

pattern_data <- data.table(
  x = x + offset_x,
  y = y + offset_y,
  group = rep(1:(grid_size*2), length.out = length(x)),
  size = abs(sin(x/5) * cos(y/5)),
  angle = (x + y)/10 + rotation
)

ggplot(pattern_data, aes(x, y)) +
  geom_curve(aes(xend = x + cos(angle),  yend = y + sin(angle), group = group), curvature = -0.2, size = 0.2, alpha = 0.3, color = "grey80") +
  geom_text(aes(size = size, color = group, label = "◀", angle = angle * 180/pi)) +
  scale_color_gradientn(colors = c("grey20", "grey97")) +
  scale_size_continuous(range = c(0.1, 5)) +
  coord_fixed() +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey9", color = NA)
  )
