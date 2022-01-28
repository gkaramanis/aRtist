library(magick)
library(dplyr)
library(ggplot2)
library(camcorder)

gg_record(dir = "genuary/2022/genuary-temp", device = "png", width = 6.5, height = 10, units = "in", dpi = 320)

path <- here::here("mitosis", "original", "georgios.jpg")

# Read image and get dimensions
img <- image_read(path)
h = image_info(img)$height
w = image_info(img)$width

# Size of tiles is going to be r * 2
r = 300 
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


img <- final %>%
  image_convert(colorspace = "gray")

# Get dimensions
img_w <- image_info(img)$width
img_h <- image_info(img)$height
img_ratio <- img_w / img_h

# Resize the longest dimension to 120 pixels, may need adjustment depending on image, higher value means more points and more "detail"
if (img_w >= img_h) {
  img <- image_resize(img, "300")
} else {
  img <- image_resize(img, ("x300"))
}

# Create array and number rows and columns
img_array <- drop(as.integer(img[[1]]))
rownames(img_array) <- 1:nrow(img_array)
colnames(img_array) <- 1:ncol(img_array)

# Create data frame from array and rename columns
threshold <- 0.5

img_df <- as.data.frame.table(img_array) %>% 
  `colnames<-`(c("y", "x", "b")) %>% 
  mutate(
    across(everything(), as.numeric),
    # map b (0-255) to bf (1-0), so that "brighter" values become smaller numbers
    bf = 1 - b / 255
  ) %>%
  arrange(y, x) %>% 
  filter(bf > threshold)

ggplot(img_df) +
  geom_point(aes(x, -y, color = bf), size = 0.5) +
  scale_color_gradient2() +
  coord_cartesian(expand = FALSE) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey97", color = NA)
  )
