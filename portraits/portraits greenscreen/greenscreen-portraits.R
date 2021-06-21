library(ggplot2)
library(dplyr)
library(magick)

# Read in image and convert to grayscale
img_name <- "keanu"

img <- image_read(paste0("portraits/images/", img_name, ".jpg")) %>%
  image_convert(colorspace = "gray")

# Get dimensions
img_w <- image_info(img)$width
img_h <- image_info(img)$height
img_ratio <- img_w / img_h

# Resize the longest dimension to 120 pixels
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
    bf = 1 - b / 255,
  ) %>%
  arrange(y, x)

ggplot(img_df) +
  geom_tile(aes(x = x, y = y, width = bf, height = 1)) + # switch 1 and bf for vertical/horizontal bars
  scale_y_reverse() +
  coord_fixed(expand = FALSE) +
  theme_void() +
  theme(plot.background = element_rect(fill = "green", color = NA))  

ggsave(paste0("portraits/portraits greenscreen/plots/", img_name, "-green-v.png"), width = 8, height = 8 / img_ratio)
