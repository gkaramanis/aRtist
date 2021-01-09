library(tidyverse)

set.seed(299)

n = 250

x1 = runif(1, -n, n) * 0.8
y1 = runif(1, -n, n) * 0.8

x2 = runif(1, -n, n) * 0.8
y2 = runif(1, -n, n) * 0.8

x3 = runif(1, -n, n) * 0.8
y3 = runif(1, -n, n) * 0.8

interf <- data.frame(x = -n:n, y = -n:n) %>% 
  expand(x, y) %>% 
  mutate(
    d1 = round(sqrt((x1 - x)^2 + (y1 - y)^2)) %% 12:20,
    d2 = round(sqrt((x2 - x)^2 + (y2 - y)^2)) %% 12:20,
    d3 = round(sqrt((x3 - x)^2 + (y3 - y)^2)) %% 12:20,
    d = (d1 + d2 + d3)^1.2
    )

ggplot(interf) +
  geom_tile(aes(x, y, fill = d)) +
  scale_fill_gradient(low = "grey10", high = "orange") +
  coord_fixed() +
  theme_void() +
  theme(
    legend.position = "none"
    # plot.background = element_rect(fill = "white")
  ) +
  ggsave(here::here("genuary", "2021", "2021-9", "2021-9.png"), dpi = 320, width = 7, height = 7)
