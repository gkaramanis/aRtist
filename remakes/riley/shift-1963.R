library(tidyverse)

shift <- expand.grid(c = 1:10, r = 1:40) %>% 
  mutate(n = row_number()) %>% 
  rowwise() %>% 
  mutate(
    x = list(4 * c(c, c + 1, c + 0.5)),
    y = list(c(r, r, r - 1))
  ) %>% 
  unnest(c(x, y))

ggplot(shift) +
  geom_polygon(aes(x, y, group = n)) +
  coord_fixed() +
  theme_void()


## Calculations

# # l1 = 9, 18
# # l2 = 13, 39
# 
# m1 = (9 - 18) / (10 - 1)
# m2 = (13 - 39) / (10 - 1)
# # y = mx+b
# 
# lns <- expand.grid(c = 1:10, r = 1:40) %>% 
#   mutate(
#     y1 = m1 * c + 31,
#     y2 = m2 * c + 31
#   )
# 
# ggplot(lns) +
#   geom_point(aes(c, r), color = "grey80") +
#   geom_point(aes(c, y1)) +
#   geom_point(aes(c, y2)) +
#   # annotate("segment", x = 1, xend = 10, y = 40-9, yend = 40-18) +
#   # annotate("segment", x = 1, xend = 10, y = 40-13, yend = 40-39) +
#   coord_fixed(ratio = 0.25) +
#   ylim(1, 40) +
#   theme_minimal()
