library(tidyverse)
library(camcorder)

gg_record(here::here("genuary/genuary-temp/"), width = 8, height = 8, dpi = 320)

# Set seed for reproducibility
set.seed(999)

# Enhanced subdivision function
subdivide <- function(x, y, width, height, depth = 0, max_depth = 4) {
  # Calculate distance from center (0.5, 0.5) to current rectangle's center
  center_x <- x + width/2
  center_y <- y + height/2
  dist_from_center <- sqrt((center_x - 0.5)^2 + (center_y - 0.5)^2)
  
  # Adjust max_depth based on distance from center
  # More subdivisions near center, fewer at edges
  local_max_depth <- max_depth + round(runif(1, -1, 1)) - round(dist_from_center * 8)
  
  if (depth >= local_max_depth || (width < 0.05 && height < 0.05)) {
    # Add some rotation to the final shapes
    # More rotation for shapes further from center
    angle <- runif(1, -15, 15) * (1 + dist_from_center)
    
    return(tibble(
      x = x,
      y = y,
      width = width,
      height = height,
      depth = depth,
      id = runif(1),
      angle = angle,
      center_x = center_x,
      center_y = center_y,
      visible = TRUE,
      dist_from_center = dist_from_center
    ))
  }
  
  # Adjust split probabilities based on distance from center
  # Prefer more complex splits near center
  split_probs <- if(dist_from_center < 0.3) {
    c(0.3, 0.3, 0.4)  # More diagonal splits near center
  } else {
    c(0.4, 0.4, 0.2)  # More regular splits away from center
  }
  
  split_type <- sample(c("vertical", "horizontal", "diagonal"), 
                      1, 
                      prob = split_probs)
  
  # Vary split points based on distance from center
  position_factor <- sin(x * pi) * cos(y * pi)
  split_range <- 0.2 + abs(position_factor) * 0.3 * (1 - dist_from_center)  # More variation near center
  
  split_point <- runif(1, 0.5 - split_range, 0.5 + split_range)
  
  # Create current rectangle with visible = FALSE since it will be split
  current_rect <- tibble(
    x = x,
    y = y,
    width = width,
    height = height,
    depth = depth,
    id = runif(1),
    angle = 0,
    center_x = center_x,
    center_y = center_y,
    visible = FALSE,
    dist_from_center = dist_from_center
  )
  
  children <- if (split_type == "vertical") {
    bind_rows(
      subdivide(x, y, width * split_point, height, depth + 1, max_depth),
      subdivide(x + width * split_point, y, width * (1 - split_point), height, depth + 1, max_depth)
    )
  } else if (split_type == "horizontal") {
    bind_rows(
      subdivide(x, y, width, height * split_point, depth + 1, max_depth),
      subdivide(x, y + height * split_point, width, height * (1 - split_point), depth + 1, max_depth)
    )
  } else {
    # Diagonal split - create four smaller rectangles
    mid_x <- x + width * split_point
    mid_y <- y + height * split_point
    
    bind_rows(
      subdivide(x, y, width * split_point, height * split_point, depth + 1, max_depth),
      subdivide(mid_x, y, width * (1 - split_point), height * split_point, depth + 1, max_depth),
      subdivide(x, mid_y, width * split_point, height * (1 - split_point), depth + 1, max_depth),
      subdivide(mid_x, mid_y, width * (1 - split_point), height * (1 - split_point), depth + 1, max_depth)
    )
  }
  
  bind_rows(current_rect, children)
}

# Generate the subdivision data
rectangles <- subdivide(0, 0, 1, 1, max_depth = 12)  # Increased max_depth for more detail

# Create the visualization with only visible rectangles
ggplot() +
  # Base rectangles
  geom_rect(data = filter(rectangles, visible),
    aes(
      xmin = x,
      ymin = y,
      xmax = x + width,
      ymax = y + height,
      fill = id
    ),
    alpha = 0.9,
    linewidth = 0.3, color = "grey9"
  ) +
  # Add some rotation to smaller rectangles
  geom_rect(data = filter(rectangles, visible & (width < 0.1 | height < 0.1)),
    aes(
      xmin = x - width/2,
      ymin = y - height/2,
      xmax = x + width/2,
      ymax = y + height/2,
      fill = id
    ),
    alpha = 0.8,
    linewidth = 0.3, color = "grey9"
  ) +
  scale_fill_gradientn(
    colors = c("grey9", "white"),  # Reversed color scheme
    guide = "none"
  ) +
  coord_fixed() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA)
  )
