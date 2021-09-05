library(tidyverse)
library(ggfx)
library(ggimage)
library(magick)

url <- "https://images.unsplash.com/flagged/photo-1556743749-bb5642435a63?ixid=MXwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHw%3D&ixlib=rb-1.2.1&auto=format&fit=crop&w=800&q=80"

img <- image_read(url)
h = image_info(img)$height
w = image_info(img)$width

blocks <- data.frame(n = 1:60) %>% 
  rowwise() %>% 
  mutate(
    x = runif(1, 0, w),
    y = runif(1, 0, h),
    s = runif(1, 40, 100)
  )

ggplot() +
  geom_image(aes(x = w/2, y = h/2, image = url), size = 1, asp = w/h) +
  as_reference(
    geom_tile(data = blocks, aes(x, y, width = s, height = s), fill = "black"),
    id = "blocks"
  ) +
  with_shadow(
    with_blend(
      geom_image(aes(x = w/2, y = h/2, image = url), size = 1.2, asp = w/h),
      bg_layer = "blocks",
      blend_type = "in"
    ),
    sigma = 5
    ) +
  coord_fixed(xlim = c(0, w), ylim = c(0, h)) +
  theme_void()

ggsave(here::here("blocks", "plots", "blocks.png"), dpi = 320, width = w, height = h, units = "px")