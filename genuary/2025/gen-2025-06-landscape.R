library(tidyverse)
library(camcorder)

gg_record(here::here("genuary/genuary-temp/"), width = 8, height = 8, dpi = 320)

set.seed(2025)

# Mountains
curves <- data.frame(x = seq(0, 10, 0.1)) %>% 
  mutate(
    y3 = 7 + 
      cos(x * runif(1, 0.5, 0.8)) * 1.2 + 
      sin(x * runif(1, 0.1, 0.3)) * 0.2,
    
    y2 = 5 + 
      cos(x * runif(1, 0.4, 0.5)) * 0.8 + 
      sin(x * runif(1, 0.2, 0.3)) * 0.5,
    
    y1 = 3
  )

# Sun
x0 = runif(1, 1, 9)
y0 = 8.5
r = 1

# Create triangle grid and colors
triangles <- expand.grid(x = seq(0, 10, 0.2), y = seq(0, 10, 0.2)) %>% 
  left_join(curves) %>% 
  fill(y1, y2, y3) %>% 
  mutate(
    color = case_when(
      ((x - x0)^2 + (y - y0)^2 <= r^2) ~ "white",
      y > y3 ~ "grey60",
      between(y, y2, y3) ~ "grey45",
      between(y, y1, y2) ~ "grey25",
      y < y1 ~ "grey90"
    )
  )

ggplot() +
  geom_text(data = triangles, aes(x, y, label = "▲", color = color)) +
  scale_color_identity() +
  coord_fixed(xlim = c(0, 10), ylim = c(0, 10)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "grey9", color = NA)
  )
