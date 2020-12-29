library(ggplot2)
library(dplyr)
library(tidyr)
library(magick)
library(gganimate)
library(av)

splitbar <- function(img) {
  # Read in image and convert to grayscale
  img <- image_read(paste0("split-bar/images/", img, ".jpg")) %>%
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
  as.data.frame.table(img_array) %>% 
    `colnames<-`(c("y", "x", "b")) %>% 
    mutate(
      across(everything(), as.numeric),
      # convert b (0-255) to bf (1-0), so that "brighter" values become smaller bars
      bf = 1 - b / 255
    )
  }

img1 <- splitbar("keanu") %>% 
  filter(x <= 59)
img2 <- splitbar("audrey") %>% 
  select(x, y, "b2" = b, "bf2" = bf)

img_j <- left_join(img1, img2) %>% 
  select(-b, -b2) %>% 
  pivot_longer(cols = c(bf, bf2), names_to = "bf")

# Colors, fill and background
col_fill <- "black"
col_bg <- "#F1E34C"

p <- ggplot(img_j) +
  geom_rect(aes(xmin = x, xmax = x + value * 0.9, ymin = y, ymax = y + 0.85), fill = col_fill, color = NA) +
  scale_y_reverse() +
  coord_fixed(expand = FALSE) +
  theme_void() +
  theme(legend.position = "none", plot.background = element_rect(fill = col_bg, color = NA)) 

p

animate(
  p +
  transition_states(bf) +
  ease_aes("cubic-in-out"),
  renderer = av_renderer("portraits/portraits split-bar/plots/split-animated.mp4"), res = 35, height = 800
)
