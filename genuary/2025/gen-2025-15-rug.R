library(ggplot2)
library(camcorder)

gg_record(here::here("genuary/genuary-temp/"), width = 8, height = 8, dpi = 320)

data.frame(
  x = cos(seq(0, 3*pi, length.out = 500)),
  y = 1:100
) |>
  ggplot() +
  geom_density_2d_filled(aes(x, y), bins = 20, n = 80, color = "black", linewidth = 0.5) +
  scale_fill_manual(values = rep(c("black", "white"), 10)) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "black", color = NA)
  )