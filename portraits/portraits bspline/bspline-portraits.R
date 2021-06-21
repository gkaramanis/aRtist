library(dplyr)
library(tidyr)
library(magick)
library(ggforce)

# Read in image and convert to grayscale
img <- image_read("portraits/images/georgios.jpg") %>%
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
    # convert b (0-255) to bf (1-0), so that "brighter" values become smaller points
    bf = 1 - b / 255,
		n = row_number()
  ) %>%
	group_by(n) %>%
	mutate(
		bx = list(c(x, x + bf * runif(1, 1, 3), x + bf * runif(1, 1, 3), x)),
		by = list(c(y + bf * runif(1, 1, 3), y + bf * runif(1, 1, 3), y, y))
	) %>%
	ungroup() %>%
	unnest(c(bx, by))

# Colors, fill and background
col_fill <- "black"
col_bg <- "#F1E34C"

ggplot(img_df) +
  geom_bspline_closed(aes(x = bx, y = by, group = n, alpha = bf), fill = col_fill, color = NA, size = 0.3) +
  scale_y_reverse() +
  scale_alpha_identity() +
  # coord_fixed(expand = FALSE) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = col_bg, color = NA)) 

ggsave("portraits/portraits bspline/plots/georgios-bspl.png", width = 8, height = 8 / img_ratio)

