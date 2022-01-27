library(ggforce)
library(tidyverse)
library(camcorder)

gg_record(dir = "genuary/2022/genuary-temp", device = "png", width = 10, height = 10, units = "in", dpi = 320)

pal <- sample(c("#2E294E", "#541388", "#F1E9DA", "#FFD400", "#D90368"))

pnts <- expand.grid(
  x = seq(0, 42, 2.8),
  y = seq(0, 42, 2.8)
  ) %>% 
  mutate(
    col = sample(c(pal, rep(NA, 2)), n(), replace = TRUE)
    )

r1 = 2.5
r2 = 1.3

s = 0.5

ggplot(pnts) +
  geom_regon(aes(x0 = x, y0 = y, angle = 0, sides = 8, r = r1, fill = col), color = NA) +
  geom_regon(aes(x0 = x, y0 = y, angle = 0, sides = 8, r = r2, fill = col), color = NA, size = 1) +
  geom_regon(aes(x0 = x, y0 = y, angle = 0, sides = 8, r = r1), size = s, fill = NA, color = "grey10") +
  geom_regon(aes(x0 = x, y0 = y, angle = 0, sides = 8, r = r2), size = s, fill = NA, color = "grey10") +
  scale_fill_identity() +
  coord_fixed() +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#989595", color = NA)
  )

