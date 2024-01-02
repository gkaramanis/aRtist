library(tidyverse)
library(ambient)
library(camcorder)

gg_record(here::here("genuary/genuary-temp/"), width = 8, height = 8, dpi = 320)

set.seed(99)

n <- 100

tiles <- expand.grid(x = seq(0, n, length.out = 70), 
                     y = seq(0, n, length.out = 70)) %>% 
  mutate(
    c = gen_worley(x, y, value = "distance", frequency = 0.1, seed = 99),
    mc = c / max(c),
    rgbc = rgb(mc, mc, mc)
    )

ggplot(tiles) +
  geom_tile(aes(x = x, y = y, fill = rgbc), color = "black", linewidth = 0.05) +
  scale_fill_identity() +
  coord_polar() +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey15", color = NA)
  )
