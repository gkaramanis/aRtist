library(tidyr)
library(dplyr)
library(ggplot2)


curves <- tribble(
  ~n, ~name, ~x, ~y,
  1, "lu", c(0, 0.5, 0), c(1, 1, 0.5),
  1, "rd", c(0.5, 1, 1), c(0, 0.5, 0),
  2, "ld", c(0, 0.5, 0), c(0, 0, 0.5),
  2, "ru", c(0.5, 1, 1), c(1, 1, 0.5)
)

t = 625

tiles <- data.frame(
  col = rep(1:sqrt(t), sqrt(t)),
  row = rep(1:sqrt(t), each = sqrt(t)),
  n = sample(c(1, 2), replace = TRUE, size = t)
) %>% 
  left_join(curves) %>% 
  unnest(c(x, y))

c1 = "#54162F"
c2 = "#F2AC88"

ggplot(tiles) +
  geom_polygon(aes(col + x, row + y, group = interaction(row, col, name)), size = 0.25, color = c1, fill = c1) +
  coord_fixed() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = c2, color = NA)
  ) +
  ggsave(here::here("truchet", "plots", "truchet-corner-triangles.png"), dpi = 320, height = 8, width = 8)
