library(ggforce)
library(camcorder)

gg_record(dir = "genuary/2022/genuary-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

xx <- 20
yy <- xx * 0.8

pnts <- expand.grid(
  x = c(-xx/2, xx/2),
  y = c(-yy/2, yy/2),
  r = seq(0, sqrt(xx^2 + yy^2), by = 0.674)
  )


# Version 1
ggplot(pnts) +
  geom_circle(aes(x0 = 0, y0 = 0, r = r), size = 0.2) +
  geom_circle(aes(x0 = x, y0 = y, r = r), size = 0.2) +
  coord_fixed(xlim = c(-xx/2, xx/2), ylim = c(-yy/2, yy/2), expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "grey97", color = NA)
  )


# Version 2
rr <- 8

ggplot(pnts) +
  geom_ellipse(aes(x0 = 0, y0 = 0, a = r, b = r * 0.4, angle = r/rr, m1 = 2.7), size = 0.2) +
  coord_fixed(xlim = c(-xx/2, xx/2), ylim = c(-yy/2, yy/2), expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "grey97", color = NA)
  )
