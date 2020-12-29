library(tidyr)
library(dplyr)
library(ggplot2)


triangles <- tribble(
  ~n, ~name, ~x, ~y,
  1, "lu", c(0, 1, 0), c(0, 1, 1),
  2, "rd", c(0, 1, 1), c(0, 0, 1),
  3, "ld", c(0, 1, 0), c(0, 0, 1),
  4, "ru", c(0, 1, 1), c(1, 1, 0)
)

t = 100

tiles <- data.frame(
  col = rep(1:sqrt(t), sqrt(t)),
  row = rep(1:sqrt(t), each = sqrt(t)),
  n = sample(c(1, 2, 3, 4), replace = TRUE, size = t)
  ) %>% 
  left_join(triangles) %>% 
  unnest(c(x, y))

ggplot(tiles) +
  geom_polygon(aes(col + x, row + y, group = interaction(col, row)), fill = "grey20", color = "grey20") +
  coord_fixed() +
  theme_void() +
  ggsave(here::here("truchet", "plots", "truchet.png"), dpi = 320, height = 8, width = 8)
