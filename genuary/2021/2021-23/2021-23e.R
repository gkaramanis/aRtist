library(ggplot2)
library(dplyr)
library(magick)
library(ggridges)


# polygon hatch group by color

# Read in image and convert to grayscale
img_url <- "https://images.unsplash.com/photo-1521119989659-a83eee488004?ixid=MXwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHw%3D&ixlib=rb-1.2.1&auto=format&fit=crop&w=1264&q=80"

img <- image_read(img_url) %>%
  image_convert(colorspace = "gray")

# Get dimensions
img_w <- image_info(img)$width
img_h <- image_info(img)$height
img_ratio <- img_w / img_h

# Resize the longest dimension to 400 pixels
if (img_w >= img_h) {
  img <- image_resize(img, "400")
} else {
  img <- image_resize(img, ("x400"))
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
    c = round(b / 60)
  )

pal <- c("#264653", "#2a9d8f", "#e9c46a", "#f4a261", "#e76f51")

ggplot(img_df) +
  geom_tile(aes(x + c * 25, y, height = 1.2, fill = factor(c)), alpha = 1) +
  scale_y_reverse() +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  coord_cartesian(expand = FALSE) +
  # facet_wrap(vars(c), nrow = 1) +
  theme_void() +
  theme(
    legend.position = "none",
    strip.text = element_blank(),
    plot.background = element_rect(fill = "grey20", color = NA)
  ) +
  ggsave(here::here("genuary", "2021", "2021-23", "2021-23e.png"), dpi = 320, width = 7, height = 7.7)
             