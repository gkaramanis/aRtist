library(ggfx)
library(magick)
library(grid)
library(tidyverse)

# See https://source.unsplash.com for more options, or use your own images
img1 <- image_read("https://source.unsplash.com/random/300x200")
Sys.sleep(1)
img2 <- image_read("https://source.unsplash.com/random/300x200")

w = 0.012

bars <- data.frame(n = seq(0, 1, by = 0.025)) %>%
  rowwise() %>% 
  mutate(
    x = list(c(n - runif(1, 0, w), n + runif(1, 0, w), n + runif(1, 0, w), n - runif(1, 0, w))),
    y = list(c(0, 0, 1, 1))
    ) %>% 
  ungroup() %>% 
  unnest(c(x, y))
  

ggplot() +
  annotation_custom(rasterGrob(img1)) +
  as_reference(
    geom_polygon(data = bars, aes(x = x, y = y, group = n), fill = "black", color = NA),
    id = "stripes"
  ) +
  with_blend(
    annotation_custom(rasterGrob(img2)),
    bg_layer = "stripes",
    blend_type = 'xor'
  ) +
  coord_fixed(ratio = 0.67, xlim = c(0, 1), expand = FALSE, clip = "off") +
  theme_void() +
  theme(plot.margin = margin(-2, -2, -2, -2)) +
  ggsave(here::here("collage", "collage-bars", paste0("collage-bars", "-", round(runif(1, 1, 10000)), ".png")), dpi = 320, width = 6, height = 4)
  
