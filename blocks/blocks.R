library(magick)
library(tidyverse)
library(grid)

url <- "https://images.unsplash.com/flagged/photo-1556743749-bb5642435a63?ixid=MXwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHw%3D&ixlib=rb-1.2.1&auto=format&fit=crop&w=800&q=80"

img <- image_read(url)

h = 100
w = 100

img_lst <- lapply(seq_along(1:30), function(i){
  x = runif(1, 0, image_info(img)$width)
  y = runif(1, 0, image_info(img)$height)
  image_crop(image_background(img, "purple", flatten = FALSE), paste0(h ,"x", w, "+", x, "+", y), repage = FALSE)
  })
  
blocks <- Reduce(`c`, img_lst) %>%
  image_mosaic()

blocks
