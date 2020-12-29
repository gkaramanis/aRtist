library(dplyr)
library(magick)
library(ggforce)

# Read in image and convert to grayscale
img_name <- "gaga"

img <- image_read(paste0("portraits/images/", img_name, ".jpg")) %>%
  image_convert(colorspace = "gray")

# Get dimensions
img_w <- image_info(img)$width
img_h <- image_info(img)$height
img_ratio <- img_w / img_h

# Resize the longest dimension to 80 pixels
if (img_w >= img_h) {
  img <- image_resize(img, "80")
} else {
  img <- image_resize(img, ("x80"))
}

# Create array and number rows and columns
img_array <- drop(as.integer(img[[1]]))
rownames(img_array) <- 1:nrow(img_array)
colnames(img_array) <- 1:ncol(img_array)

# Create data frame from array and rename columns
img_df <- as.data.frame.table(img_array) %>% 
  `colnames<-`(c("y", "x", "b")) %>% 
  mutate(
    across(everything(), as.numeric),
    # map b (0-255) to bf (1-0), so that "brighter" values become smaller numbers
    bf = 1 - b / 255,
  ) %>%
  arrange(y, x)

arcs <- img_df %>% 
  mutate(
    x0 = max(img_df$x) / 2,
    y0 = max(img_df$y) / 2
  ) %>% 
  rowwise() %>% 
  mutate(
    r = sqrt((y - y0) ^ 2 + (x - x0) ^ 2),
    a = atan((y - y0) / (x - x0)) - pi/2,
    a = if_else(y < y0 & x > x0, a - pi, a),
    a = if_else(y > y0 & x > x0, a + pi, a)
  )

ggplot(arcs) +
  geom_arc(aes(x0 = x0, y0 = y0, r = r, start = a - 2/r, end = a + 2/r, size = bf)) +
  # geom_spoke(aes(x = x, y = y, angle = a, size = bf * 2), radius = 1) +
  scale_y_reverse() +
  scale_size_identity() +
  coord_fixed() +
  theme_void() +
  ggsave(paste0("portraits/portraits arcs/plots/", img_name, ".png"))
