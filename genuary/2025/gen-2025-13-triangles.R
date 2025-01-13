library(dplyr)
library(ggplot2)
library(camcorder)

gg_record(here::here("genuary/genuary-temp/"), width = 8, height = 8, dpi = 320)

set.seed(919)

expand.grid(
  x = rnorm(20),
  y = rnorm(15)
) %>% 
  ggplot(aes(x, y)) +
  ggforce::geom_delaunay_segment(lineend = "round", color = "grey90",  linewidth = 1.3) +
  ggforce::geom_delaunay_segment2(aes(color = after_stat(index)), lineend = "round", n = 50) +
  scale_color_gradientn(colors = c("grey0", "grey99")) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey25", color = NA)
  )
  
