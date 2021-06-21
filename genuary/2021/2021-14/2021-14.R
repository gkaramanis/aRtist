library(tidyverse)
library(ggforce)
library(poissoned)
library(wesanderson)
library(colorspace)

s <- round(runif(1, 0, 1000))

set.seed(s)

pnts1 <- data.frame(
  x = rnorm(10, 0, 100),
  y = rnorm(10, 0, 100)
) %>% 
  expand(x, y)

pnts2 <- poisson_disc(ncols = 10, nrows = 20, cell_size = 10, verbose = TRUE)

pnts <- rbind(pnts1, pnts2) %>% 
  mutate(fill = round((x / y) %% 4))

w <- max(pnts$x) - min(pnts$x)
h <- max(pnts$y) - min(pnts$y)
a <- w/h

pal = wes_palette("Darjeeling1", 5, "discrete")

ggplot(pnts, aes(x, y)) +
  geom_voronoi_tile(aes(fill = factor(fill))) +
  geom_voronoi_segment(aes(color = factor(fill)), size = 0.4, alpha = 0.9) +
  scale_fill_manual(values = pal) +
  scale_color_manual(values = lighten(pal, 0.2)) +
  # coord_fixed(expand = FALSE) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey90", color = NA),
    plot.margin = margin(10, 10, 10, 10)
  ) 

ggsave(here::here("genuary", "2021", "2021-14", paste0("2021-14-", s, ".png")), dpi = 320, width = 7, height = 7 / a)
