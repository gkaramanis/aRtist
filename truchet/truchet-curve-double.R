library(tidyr)
library(dplyr)
library(ggforce)


curves <- tribble(
  ~n, ~name, ~x, ~y,
  1, "lu", c(0, 0.5, 0.5), c(0.5, 0.5, 1),
  1, "rd", c(0.5, 0.5, 1), c(0, 0.5, 0.5),
  2, "ld", c(0, 0.5, 0.5), c(0.5, 0.5, 0),
  2, "ru", c(0.5, 0.5, 1), c(1, 0.5, 0.5)
)

t = 900

tiles <- data.frame(
  col = rep(1:sqrt(t), sqrt(t)),
  row = rep(1:sqrt(t), each = sqrt(t)),
  n = sample(c(1, 2), replace = TRUE, size = t)
) %>% 
  left_join(curves) %>% 
  unnest(c(x, y))

c1 = "#00203FFF"
c2 = "#ADEFD1FF"

ggplot(tiles) +
  geom_bspline(aes(col + x, row + y, group = interaction(row, col, name)), size = 2, n = 15, color = c1, lineend = "round") +
  coord_fixed() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = c2, color = NA)
  ) +
  ggsave(here::here("truchet", "plots", "truchet-curve-double.png"), dpi = 320, height = 8, width = 8)
