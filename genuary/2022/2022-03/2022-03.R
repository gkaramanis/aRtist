library(ggshadow)

pal <- MetBrewer::met.brewer("Hiroshige")

n = 70

stars <- data.frame(
  x = runif(n * 2),
  y = runif(n * 2),
  size = runif(n * 2, 0, 0.2),
  x2 = runif(n),
  y2 = runif(n),
  size2 = runif(n, 0, 3),
  color2 = pal[runif(n, 5, 10)]
)

ggplot(stars) +
  geom_point(aes(x, y, size = size), color = "grey67") +
  geom_glowpoint(aes(x2, y2, size = size2, color = color2, shadowsize = size2 ^ 1.2)) +
  scale_size_continuous(range = c(0.1, 4)) +
  scale_color_identity() +
  # scale_color_gradientn(colors = pal) +
  coord_fixed(clip = "off") +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey10", color = NA)
  )
  