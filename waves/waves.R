library(ggstream)
library(tidyverse)
# library(wesanderson)
library(vhs)

m = 100

waves <- data.frame(x = 1:m, color = sample(1:5)) %>% 
  rowwise() %>% 
  mutate(
    v = 0.15 + runif(1, max = 0.025)
    ) %>% 
  ungroup()

# pal <- sample(wes_palette("Zissou1", 100, type = "continuous"))
pal <- sample(vhs("vhs", type = "c", n = 100))

ggplot(waves) +
  geom_stream(aes(x = x, y = v, color = colorspace::darken(factor(v)), fill = factor(v)), type = "proportional", bw = 1, extra_span = 0.9, size = 0.2) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  coord_cartesian(expand = FALSE) +
  theme_void() +
  theme(
    legend.position = "none"
    ) +
  ggsave(here::here("waves", "waves.png"), dpi = 320, width = 5, height = 5)

