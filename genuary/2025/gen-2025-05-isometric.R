library(tidyverse)
library(camcorder)

gg_record(here::here("genuary/genuary-temp/"), width = 8, height = 8, dpi = 320)

set.seed(99)

# Simplified projection and shape creation
angle <- pi/6
iso_project <- function(x, y, z) {
  list(x = (x - y) * cos(angle), y = (x + y) * sin(angle) - z)
}

create_parallelogram <- function(x, y, z, size = 1, height = NULL) {
  height <- if (is.null(height)) size * 9 else height
  faces <- list(
    list(points = list(c(x, y, z + height), c(x + size, y, z + height),
                      c(x + size, y + size, z + height), c(x, y + size, z + height)), face = "top"),
    list(points = list(c(x + size, y, z), c(x + size, y + size, z),
                      c(x + size, y + size, z + height), c(x + size, y, z + height)), face = "right"),
    list(points = list(c(x, y, z), c(x, y + size, z),
                      c(x, y + size, z + height), c(x, y, z + height)), face = "left")
  )
  map_dfr(faces, ~bind_cols(
    map_dfr(.x$points, ~as_tibble(iso_project(.x[1], .x[2], .x[3]))),
    face = .x$face
  ))
}

# Define pattern layout points
layout_points <- tibble(
  x = c(0, 2, 4, 6, 8, 10, 6, 6, 6, 4, 4, 4),
  y = c(0, 2, 4, 6, 8, 10, 6, 8, 10, 4, 6, 8)
)

# Calculate distance to nearest layout point
dist_to_layout <- function(x, y, layout_points) {
  pmap_dbl(layout_points, ~sqrt((x - ..1)^2 + (y - ..2)^2)) %>% min()
}

# Generate grid and heights
grid_data <- expand.grid(x = seq(0, 9 * 1.2, by = 1.2),
                        y = seq(0, 9 * 1.2, by = 1.2)) %>% 
  as_tibble() %>%
  mutate(
    dist = map2_dbl(x, y, ~dist_to_layout(.x, .y, layout_points)),
    height = case_when(
      dist < 1.2 ~ -2,
      dist < 2.0 ~ runif(n(), -7, 2),
      TRUE ~ runif(n(), 3, 9)
    )
  )

# Create shapes and plot
shape_data <- pmap_dfr(
  list(grid_data$x, grid_data$y, grid_data$height),
  ~create_parallelogram(..1, ..2, 0, 0.5, ..3),
  .id = "shape_id"
)

# Plot
ggplot(shape_data) +
  geom_polygon(aes(x, y, group = interaction(shape_id, face)), fill = "grey7", color = "white", linewidth = 0.5) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "grey95", color = NA)) 