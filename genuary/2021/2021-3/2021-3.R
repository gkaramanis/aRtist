library(ggplot2)
library(dplyr)
library(magick)
library(ggridges)

# Read in image and convert to grayscale
img <- image_read(here::here("genuary", "2021", "2021-3", "images", "gaga.jpg")) %>%
  image_convert(colorspace = "gray")

# Get dimensions
img_w <- image_info(img)$width
img_h <- image_info(img)$height
img_ratio <- img_w / img_h

# Resize the longest dimension to 160 pixels
if (img_w >= img_h) {
  img <- image_resize(img, "160")
} else {
  img <- image_resize(img, ("x160"))
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
    n = row_number()
  ) %>%
   filter(n %% 2 == 0)

# Colors, fill and background
col_fill <- "black"
col_bg <- "white"

ggplot(img_df) +
  geom_ridgeline_gradient(aes(x, y, height = b/50, group = y, fill = b), color = "grey30", size = 0.25) +
  scale_y_reverse() +
  scale_fill_viridis_c(option = "magma") +
  coord_cartesian(expand = FALSE) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = col_fill, color = NA)
    )  +
		ggsave(here::here("genuary", "2021", "2021-3", "2021-3.png"), dpi = 320, width = 7, height = 7 / img_ratio)
