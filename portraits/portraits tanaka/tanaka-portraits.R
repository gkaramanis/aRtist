library(ggplot2)
library(dplyr)
library(magick)
library(metR)

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

ggplot(img_df) +
  geom_contour_filled(aes(x, y, z = bf)) +
  geom_contour_tanaka(aes(x, y, z = bf)) +
  scale_y_reverse() +
  coord_fixed() +
  scale_fill_brewer(guide = "none") +
  theme_void() +
  theme(plot.background = element_rect(fill = "black", color = NA)) +
	ggsave(paste0("portraits/portraits tanaka/plots/", img_name, "-tanaka.png"), width = 8, height = 8 / img_ratio)
