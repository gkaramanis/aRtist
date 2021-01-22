library(tidyverse)
library(poissoned)

pnts <- poisson_disc(400, 400, 0.05)

ly = 10

pnts_f <- pnts %>% 
  filter(!between(y, 0.995 * ly, 1.005 * ly))

ggplot(pnts_f) +
  geom_point(aes(x, y, size = (x + y) %% 5, color = (y - x) %% 5)) +
  scale_size_continuous(range = c(0.05, 0.3)) +
  scale_color_gradient(high = "grey5", low = "grey60") +
  coord_fixed(expand = FALSE) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey97", color = NA)
  ) +
  ggsave(here::here("genuary", "2021", "2021-22", "2021-22.png"), dpi = 320, width = 7, height = 7)
