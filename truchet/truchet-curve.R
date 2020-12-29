library(tidyr)
library(dplyr)
library(ggforce)


curves <- tribble(
  ~n, ~name, ~x, ~y,
  1, "lu", c(0, 0.5, 0.5), c(0.5, 0.5, 1),
  2, "rd", c(0.5, 0.5, 1), c(0, 0.5, 0.5),
  3, "ld", c(0, 0.5, 0.5), c(0.5, 0.5, 0),
  4, "ru", c(0.5, 0.5, 1), c(1, 0.5, 0.5)
  )

t = 900

tiles <- data.frame(
  col = rep(1:sqrt(t), sqrt(t)),
  row = rep(1:sqrt(t), each = sqrt(t)),
  n = sample(c(1, 2, 3, 4), replace = TRUE, size = t)
) %>% 
  left_join(curves) %>% 
  unnest(c(x, y))

ggplot(tiles) +
  geom_bspline(aes(col + x, row + y, group = interaction(col, row)), size = 2, n = 15, color = "yellow", lineend = "round") +
  coord_fixed() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "purple", color = NA)
  ) +
  ggsave(here::here("truchet", "plots", "truchet-curve.png"), dpi = 320, height = 8, width = 8)
