library(tidyverse)

cc <- expand.grid(
  x = 2 * 1:8,
  y = 2 * 1:8
  ) %>% 
  rowwise() %>% 
  mutate(
    # r = sample(c(-1, 1), 1),
    r = 1,
    xp = list(r * c(0, 0.33, 1, 1)),
    yp = list(r * c(1, 0.33, 0, 1))
    ) %>% 
  unnest(c(xp, yp)) 
  

ggplot(cc) +
  geom_point(aes(x, y)) +
  geom_polygon(aes(x = x - xp, y = y - yp, group = interaction(x, y)),
               fill = "darkgreen") +
  # geom_polygon(aes(x = x - xp, y = y - yp, group = interaction(x, y)),
               # fill = "orange") +
  # geom_polygon(aes(x = x + xp, y = y - yp, group = interaction(x, y)),
               # fill = "blue") +
  coord_fixed()
