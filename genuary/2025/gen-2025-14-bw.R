library(ggplot2)
library(camcorder)

gg_record(here::here("genuary/genuary-temp/"), width = 8, height = 8, dpi = 320)

set.seed(979)

expand.grid(
  x = runif(500),
  y = runif(500)
) |>
  ggplot() +
  geom_density_2d_filled(aes(x, y), bins = 20, n = 50, color = "black", linewidth = 0.5) +
  scale_fill_manual(values = rep(c("black", "white"), 10)) +
  coord_fixed(clip = "off") +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "black", color = NA)
  )
  
