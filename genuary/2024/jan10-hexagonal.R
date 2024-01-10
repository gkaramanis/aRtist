library(tidyverse)
library(ggforce)
library(spatstat)
library(camcorder)

gg_record(here::here("genuary/genuary-temp/"), width = 8, height = 8, dpi = 320)

s <- 0.03

set.seed(99)

hg <- hexgrid(W = owin(), s = s) %>% 
  as.data.frame() %>% 
  mutate(c = sample(c(TRUE, FALSE), n(), replace = TRUE, prob = c(1, 0.33)))

ggplot(hg) +
  geom_regon(aes(x0 = x, y0 = y, r = s * 0.8, sides = 6, angle = 0, fill = c, color = !c), position = position_jitter(width = 0.003, height = 0.003, seed = 99)) +
  scale_linewidth_identity() +
  scale_color_manual(values = c("white", "black")) +
  scale_fill_manual(values = c("white", "black")) +
  coord_fixed(clip = "off") +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey15", color = NA)
  )

