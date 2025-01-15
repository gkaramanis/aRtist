library(ggplot2)
library(camcorder)

gg_record(here::here("genuary/genuary-temp/"), width = 8, height = 8, dpi = 320)

set.seed(939)

generate_palette <- function(n = 5) {
  values <- seq(0.5, 0.9, length.out = n)
  colors <- hsv(h = values + 0.1, s = values + 0.1, v = values)      
  return(colors)
}

subdivide_cell <- function(x, y, size, depth = 0, max_depth = 3) {
  if (depth < max_depth && runif(1) < 0.6) {
    sub_size <- size/2
    cells <- list()
    for(dx in c(0, 1)) {
      for(dy in c(0, 1)) {
        new_cells <- subdivide_cell(x + dx * sub_size, y + dy * sub_size, 
                                    sub_size, depth + 1, max_depth)
        cells <- c(cells, new_cells)
      }
    }
    return(cells)
  } else {
    return(list(data.frame(x = x, y = y, size = size, depth = depth)))
  }
}

grid_size <- 5

cells <- list()
for(i in 1:grid_size) {
  for(j in 1:grid_size) {
    cells <- c(cells, subdivide_cell(i, j, 1))
  }
}

grid_data <- do.call(rbind, cells)
unique_depths <- length(unique(grid_data$depth)) + 1
pal <- generate_palette(unique_depths)
grid_data$fill <- pal[grid_data$depth + 1]

ggplot(grid_data, aes(x, y)) +
  geom_tile(aes(width = size, height = size, fill = fill), color = "grey9", linewidth = 0.5) +
  scale_fill_identity() +
  coord_fixed() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "grey9", color = NA),
    legend.position = "none"
  )