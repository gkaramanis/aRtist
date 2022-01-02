library(dithr)
library(magick)
library(magrittr)

im <- image_read(here::here("genuary", "2022", "2022-01", "2022-01.png")) %>%
  image_convert(type='grayscale') %>%
  image_scale(geometry="75%")

m <- as_EBImage(im)@.Data

dithered_matrix <- dither(m, edm = diffusion_matrix$atkinson, threshold = 0.6)

plot_matrix(dithered_matrix)
