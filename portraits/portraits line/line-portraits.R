library(ggplot2)
library(dplyr)
library(tidyr)
library(magick)

# Read in image and convert to grayscale
img <- image_read("portraits/images/gaga.jpg") %>%
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
  ) %>% 
  # Create extra "steps" for the sine curves
  rowwise() %>% 
  mutate(t = list(x + seq(0, 1, by = 0.05))) %>% 
  unnest(t)

# Colors, fill and background
col_fill <- "black"
col_bg <- "#E335C2"

ggplot(img_df) +
  geom_path(aes(x = t, y = y + bf * sin(4 * pi * t) / 2, group = y), color = col_fill) +
  scale_y_reverse() +
  coord_fixed(expand = FALSE) + 
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = col_bg, color = NA)) +
  ggsave("portraits/portraits line/plots/gaga-l.png")
