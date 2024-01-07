library(ggforce)
library(gganimate)
library(camcorder)

gg_record(here::here("genuary/genuary-temp/"), width = 8, height = 8, dpi = 320)

l <- 30

arc_df <- data.frame(
  t = 1:l,
  end_a = seq(-1, 2 * pi, length.out = l)
  )

ggplot(arc_df) +
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 12, r = 0.9 * t, start = log(end_a), end = end_a), color = "white", fill = "black", alpha = 0.1, linewidth = 0.6) +
  geom_point(aes(cos(end_a) * 32, sin(end_a) * 32, alpha = end_a), color = "white", size = 1) +
  scale_alpha_continuous(range = c(-0.5, 1)) +
  coord_fixed(xlim = c(-40, 40)) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey15", color = NA)
  ) 
