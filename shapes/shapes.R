library(tidyverse)
library(ambient)

shapes <- c(21, 22, 23, 24, 25) 
colours <- c("black", "white")

grid_points <- long_grid(x = seq(0, 40), y = seq(0, 30)) %>% 
  rowwise() %>% 
  mutate(
    shape = sample(shapes, 1),
    small_shape = round(runif(1, min = 0, max = 1)),
    colour = sample(colours, 1)
    )

ggplot(grid_points) +
  geom_point(aes(x = x, y = y, shape = as.factor(shape), colour = colour), fill = NA, size = 3.5) +
  geom_point(data = subset(grid_points, small_shape == 1), aes(x = x, y = y, shape = as.factor(shape), fill = colour), size = 1) +
  coord_fixed(expand = FALSE, xlim = c(-0.5, 40.5), ylim = c(-0.5, 30.5)) +
  scale_shape_manual(values = shapes) +
  scale_color_manual(values = colours) +
  scale_fill_manual(values = colours) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "white", colour = NA)
  ) 

ggsave(here::here("shapes", "plots", "shapes.png"), dpi = 320, width = 8, height = 6) 
