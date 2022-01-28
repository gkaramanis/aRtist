library(dplyr)
library(tidyr)
library(magick)
library(ggforce)
library(camcorder)

gg_record(dir = "genuary/2022/genuary-temp", device = "png", width = 7, height = 10, units = "in", dpi = 320)

# Read in image and convert to grayscale
img <- image_read("portraits/images/georgios.jpg") %>%
  image_convert(colorspace = "gray")

# Get dimensions
img_w <- image_info(img)$width
img_h <- image_info(img)$height
img_ratio <- img_w / img_h

# Resize the longest dimension to 500 pixels
if (img_w >= img_h) {
  img <- image_resize(img, "500")
} else {
  img <- image_resize(img, ("x500"))
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
    bf = 1 - b / 255,
    i = row_number(),
    row = (y - 1) %/% 11,
    col = (x - 1) %/% 18
  ) %>%
  ungroup() %>% 
  rowwise() %>% 
  mutate(
    x = x + 1/bf,
    y = y + 1/bf
  ) %>% 
  ungroup()

ggplot(img_df) +
  geom_bspline(aes(x, -y, size = bf^2.5, group = interaction(col, row))) +
  scale_size_continuous(range = c(0.01, 0.4)) +
  coord_fixed() +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey97", color = NA)
  )

