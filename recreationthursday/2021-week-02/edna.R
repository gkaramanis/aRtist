library(tidyverse)
library(ggforce)
# library(ggimage) # To 'trace' the original

# All the curves are parts of circles with following centres:
curves <- expand.grid(
  x = 3.1 * seq(-3, 3, 2),
  y = 1.55 * seq(-5, 5, 4)
) %>% 
  rowwise() %>% 
  mutate(
    x = list(x + 1.55 * -1:3),
    y = list(y + 1.55 * -1:3)
  ) %>%
  unnest(c(x, y)) %>% 
  group_by(x)  

ggplot() +
  # uncomment to show the original image
  # geom_image(data = NULL, aes(0, 0, image = here::here("original", "twilight_wave_original.png")), size = 1) + 
  # uncomment to show the full circles
  # geom_circle(data = curves, aes(x0 = x, y0 = y, r = 2.19)) + 
  # Draw the arcs. The if_else makes the top and bottom arcs bigger
  geom_arc(data = subset(curves, x < 7), aes(x0 = x, y0 = y, r = 2.19, start = if_else(y > 7, 0, pi/4), end = if_else(y < -7, pi, 3 * pi/4)), color = "grey10") + 
  geom_arc(data = subset(curves, x > -7), aes(x0 = x, y0 = y, r = 2.19, start = if_else(y > 7, 0, -pi/4), end = if_else(y < -7, -pi, -3 * pi/4)), color = "grey10") +
  # Draw the orange circles
  geom_circle(data = NULL, aes(x0 = 0, y0 = seq(-9.3, 9.3, length.out = 7), r = 0.63), color = "grey10") +
  # Make a frame
  annotate("tile", x = 0, y = 0, width = 20, height = 20, color = "grey10", fill = NA, size = 0.5) +
  xlim(-10, 10) +
  ylim(-10, 10) +
  coord_fixed(expand = FALSE, clip = "off") +
  theme_void() +
  theme(
    plot.margin = margin(20, 20, 20, 20),
    plot.background = element_rect(fill = "grey97", color = NA)
  ) 

ggsave(here::here("plots", "edna-arcs.png"), height = 5, width = 5)
