library(ggplot2)
library(dplyr)
library(magick)

img <- image_read("animals/images/tiger.jpg") %>%
  image_convert(colorspace = "gray")

# Get dimensions
img_w <- image_info(img)$width
img_h <- image_info(img)$height

# Create array and number rows and columns
img_array <- drop(as.integer(img[[1]]))
rownames(img_array) <- 1:nrow(img_array)
colnames(img_array) <- 1:ncol(img_array)

# Create data frame from array and rename columns
img_df <- as.data.frame.table(img_array) %>% 
  `colnames<-`(c("y", "x", "b")) %>% 
  mutate(
    across(everything(), as.numeric),
    bf = 1 - b / 255
  )

# tiger 3900
n = 3900

ggplot(img_df) +
  geom_point(aes(x, y, size = bf)) +
  theme_void() +
  coord_fixed()
