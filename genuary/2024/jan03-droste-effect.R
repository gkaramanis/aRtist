library(ggforce)
library(dplyr)
library(camcorder)

gg_record(here::here("genuary/genuary-temp/"), width = 8, height = 8, dpi = 320)

n <- 40

data.frame(angle = seq(-0.05, pi, length.out = n)) %>% 
  mutate(r = seq(0, 0.9, length.out = n)) %>% 
  ggplot() +
  geom_regon(aes(x0 = 0, y0 = 0, r = log(r), sides = 3, angle = angle, linewidth = 1/r), fill = NA, color = "white") +
  scale_linewidth_continuous(range = c(0.2, 1)) +
  coord_fixed() +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey15", color = NA)
  )
  
