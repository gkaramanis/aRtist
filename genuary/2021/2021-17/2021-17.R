library(tidyverse)
library(wesanderson)

s <- round(runif(1, 0, 100000))
set.seed(s)

n = 10000

pnts <- data.frame(r = 1:n, x = rep(0, n), y = rep(0, n), a = rep(0, n))

for (i in 2:n) {
  l = rnorm(1, 3, 1)
  
  b = runif(1, 0, 30) * pi / 180
  c = -runif(1, 60, 120) * pi / 180
  
  a = sample(c(b, c), 1)
  
  pnts$a[i] <- pnts$a[i-1] + a
  pnts$x[i] <- pnts$x[i-1] + l * cos(pnts$a[i])
  pnts$y[i] <- pnts$y[i-1] + l * sin(pnts$a[i])
  
}

pal = wes_palette("Zissou1", 9, type = "continuous")

ggplot(pnts) +
  geom_path(aes(x, y, color = factor(r %% 9)), size = 0.2) +
  # geom_path(aes(x, y, color = r %% 9), size = 0.2) +
  scale_color_manual(values = pal) +
  coord_fixed(expand = FALSE) +
  xlim(-150, 150) +
  ylim(-150, 150) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey95", color = NA)
  ) 

ggsave(here::here("genuary", "2021", "2021-17", paste0("2021-17-", s, ".png")), dpi = 320, width = 7, height = 7)

