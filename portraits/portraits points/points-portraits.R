library(ggplot2)
library(dplyr)
library(magick)

# Read in image and convert to grayscale
img <- image_read("portraits/images/keanu.jpg") %>%
  image_convert(colorspace = "gray")

# Get dimensions
img_w <- image_info(img)$width
img_h <- image_info(img)$height

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
    # convert b (0-255) to bf (1-0), so that "brighter" values become smaller points
    bf = 1 - b / 255
  )

# Colors, fill and background
col_fill <- "black"
col_bg <- "#F1E34C"

ggplot(img_df) +
  geom_point(aes(x = x, y = y, size = bf), color = col_fill) +
  scale_y_reverse() +
  scale_size_continuous(range = c(0, 3)) +
  coord_fixed(expand = FALSE) +
  theme_void() +
  theme(legend.position = "none", plot.background = element_rect(fill = col_bg, color = NA))

  # ggsave("portraits/portraits points/plots/keanu-p.png", height = 8, width = 7)
