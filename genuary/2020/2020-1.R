library(ggplot2)

set.seed(77)

n <- 1000
x <- runif(n)
y <- runif(n)

for (i in seq(0, 2 * pi, by = 0.2)) {
  a = x * sin(i) - y * cos(i)
  for (j in 1:12) {
    c = y * sin(i) - x * sin(j)
    for (k in 1:5) {
      s = c + x * sin(j)
      xend <- x + y * cos(2 * k)
      yend <- y - x * sin(2 * k)
    }
  }
}

ggplot() +
  geom_tile(aes(x, y, alpha = a, width = 0.5 - s, height = 0.5 + s, fill = c)) +
  geom_tile(aes(y, x, alpha = a, width = 0.5 - c, height = 0.5 + a, fill = c)) +
  scale_fill_viridis_c(option = "magma") +
  scale_alpha_continuous(range = c(0, 0.17)) +
  coord_cartesian(clip = "off", expand = FALSE) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#8c92ac", color = NA)
  ) +
  ggsave(here::here("genuary", "2020", "2020-1.png"), dpi = 320, width = 5, height = 5)
