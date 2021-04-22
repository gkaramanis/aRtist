library(ggplot2)
library(dplyr)
library(magick)

# Read in image and convert to grayscale
img <- image_read("portraits/images/keanu.jpg")

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

# Get channels
r <- image_channel(img, channel = "red")
g <- image_channel(img, channel = "green")
b <- image_channel(img, channel = "blue")

# Here I should make a function or loop, but how?
img_array_r <- drop(as.integer(r[[1]]))
rownames(img_array_r) <- 1:nrow(img_array_r)
colnames(img_array_r) <- 1:ncol(img_array_r)

img_array_b <- drop(as.integer(b[[1]]))
rownames(img_array_b) <- 1:nrow(img_array_b)
colnames(img_array_b) <- 1:ncol(img_array_b)

img_array_g <- drop(as.integer(g[[1]]))
rownames(img_array_g) <- 1:nrow(img_array_g)
colnames(img_array_g) <- 1:ncol(img_array_g)

# Make data frame for each channel
img_df_r <- as.data.frame.table(img_array_r) %>% 
  mutate(Var3 = "red")

img_df_g <- as.data.frame.table(img_array_g) %>% 
  mutate(Var3 = "green")

img_df_b <- as.data.frame.table(img_array_b) %>% 
  mutate(Var3 = "blue")

# rbind all channel data frames
img_df <- rbind(img_df_r, img_df_g, img_df_b) %>% 
  `colnames<-`(c("y", "x", "c", "v")) %>% 
  distinct(x, y, c, v) %>% 
  mutate(
    across(c(x, y, c), as.numeric),
    
    # map v (0-255) to vf (0-1), in order to use for alpha
    vf = c / 255,
    # offset for "pixels"
    x = case_when(
      v == "green" ~ x + 0.3,
      v == "blue" ~ x + 0.6,
      TRUE ~ x
    )
  )

ggplot(img_df) +
  geom_rect(aes(xmin = x, xmax = x + 0.3, ymin = y, ymax = y + 0.9, fill = v, alpha = vf), color = NA) +
  scale_y_reverse() +
  scale_fill_identity() +
  scale_alpha_identity() +
  # Set expand to TRUE for "frame" around plot
  coord_cartesian(expand = FALSE) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "black", color = NA))

  # ggsave("portraits/portraits rgb/plots/keanu-rgb.png", width = 8, height = 8 / img_ratio)
