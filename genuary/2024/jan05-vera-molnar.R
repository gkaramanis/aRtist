library(tidyverse)
library(ggforce)
library(camcorder)

gg_record(here::here("genuary/genuary-temp/"), width = 8, height = 8, dpi = 320)

set.seed(99)

n <- 8

sq <- expand.grid(x = 1:n, y = 1:n) %>% 
  mutate(
    s = runif(n^2, 4, 4.9),
    r = runif(n^2, 0.1, 0.6)
    ) %>% 
  rowwise() %>% 
  mutate(r = list(seq(r, r + 0.2, 0.1))) %>% 
  unnest(r)
  
ggplot(sq) +
  geom_regon(aes(x0 = x, y0 = y, r = r, sides = s, angle = pi/4), fill = NA, color = "white", linewidth = 0.4) +
  geom_regon(aes(x0 = x, y0 = y, r = r + 0.05, sides = s, angle = pi/4 + 0.05), fill = NA, color = "white", linewidth = 0.4) +
  geom_regon(aes(x0 = x, y0 = y, r = r + 0.1, sides = s, angle = pi/2), fill = NA, color = "white", linewidth = 0.4) +
  coord_fixed() +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey15", color = NA)
  )

