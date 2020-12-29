library(ggplot2)
library(dplyr)
library(magick)
library(ggforce)

img_name <- "keanu"
threshold <- 0.6 # Used to discard darker pixels later 

# Read in image and convert to grayscale
img <- image_read(paste0("portraits/images/", img_name, ".jpg")) %>%
  image_convert(colorspace = "gray")

# Get dimensions
img_w <- image_info(img)$width
img_h <- image_info(img)$height
img_ratio <- img_w / img_h

# Resize the longest dimension to 120 pixels, may need adjustment depending on image, higher value means more points and more "detail"
if (img_w >= img_h) {
  img <- image_resize(img, "120")
} else {
  img <- image_resize(img, ("x120"))
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
    bf = 1 - b / 255
  ) %>%
  arrange(y, x) %>% 
  filter(bf > threshold)

# Plot, switch between for voronoi segment and tile for different effects
ggplot(img_df) +
  geom_voronoi_segment(aes(x, y), color = "black") +
  # geom_voronoi_tile(aes(x, y, fill = b), color = "black", max.radius = 5) +
  scale_y_reverse() +
  coord_fixed() +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey97", color = NA)
    ) +
  ggsave(paste0("portraits/portraits voronoi/plots/", img_name, "-voronoi-", threshold, ".png"), width = 8, height = 8 / img_ratio)
