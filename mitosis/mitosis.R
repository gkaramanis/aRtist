library(magick)
library(dplyr)

# Test images
path <- "https://images.unsplash.com/photo-1595887543484-e4a94a97abf1?ixid=MnwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8&ixlib=rb-1.2.1&auto=format&fit=crop&w=1200&q=80"

# path <- "https://images.unsplash.com/photo-1507747586703-16d5436bd01d?ixid=MnwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8&ixlib=rb-1.2.1&auto=format&fit=crop&w=1200&q=80"

# path <- here::here("mitosis", "original", "georgios.jpg")

# Read image and get dimensions
img <- image_read(path)
h = image_info(img)$height
w = image_info(img)$width

# Size of tiles is going to be r * 2
r = 100 
# Number of tiles x * y
nx = round(w / r)
ny = round(h / r)

blocks <- expand.grid(
  x = seq(0, w - 2 * r, length.out = nx),
  y = seq(0, h - 2 * r, length.out = ny)
  ) %>%
  rowwise() %>% 
  mutate(
    xy = paste0(x, "+", y),
    # crop tiles from image
    img = list(image_crop(img, geometry_area(r * 2, r * 2, x, y)))
    ) %>% 
  ungroup()

# Join tiles
img_list <- image_join(blocks$img)
final <- image_montage(img_list, paste0("x", r, "+0+0"), tile = paste0(nx, "x", ny))

# Write final image
filename <- paste0(as.numeric(Sys.time()), "_r", r, ".png")
image_write(final, path = here::here("mitosis", filename), format = "png")



