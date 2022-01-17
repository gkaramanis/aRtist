library(tidyverse)
library(MetBrewer)
library(ggforce)
library(poissoned)
library(camcorder)

gg_record(dir = "genuary/2022/genuary-temp", device = "png", width = 10, height = 10, units = "in", dpi = 320)

# Code for noise generation from:
# https://blog.djnavarro.net/posts/2021-09-07_water-colours/


# Start with Poisson-disc sampling instead of:
# points_time0 <- expand_grid(x = 1:70, y = 1:70) %>% 
# mutate(time = 0, id = row_number())

points_time0 <- poisson_disc(ncols = 120, nrows = 120, cell_size = 1, verbose = TRUE) %>% 
  mutate(time = 0, id = row_number())


field <- function(points, frequency = .1, octaves = 1) {
  ambient::curl_noise(
    generator = ambient::fracture,
    fractal = ambient::billow,
    noise = ambient::gen_simplex,
    x = points$x,
    y = points$y,
    frequency = frequency,
    octaves = octaves,
    seed = 1
  )
}
   
shift <- function(points, amount, ...) {
  vectors <- field(points, ...)
  points <- points %>%
    mutate(
      x = x + vectors$x * amount,
      y = y + vectors$y * amount,
      time = time + 1,
      id = id
    )
  return(points)
}

iterate <- function(pts, time, step, ...) {
  bind_rows(accumulate(
    .x = rep(step, time), 
    .f = shift, 
    .init = pts,
    ...
  ))
}

pts <- points_time0 %>% 
  iterate(time = 9, step = 1)

pal <- met.brewer("Austria", n = 3)

ggplot(pts) +
  geom_delaunay_segment(aes(x, y, color = factor(time %% 3)), alpha = 0.2, size = 0.6) +
  scale_color_manual(values = pal) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey97", color = NA)
  )

