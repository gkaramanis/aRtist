library(data.table)
library(ambient)
library(ggplot2)
library(camcorder)

gg_record(here::here("genuary/genuary-temp/"), width = 8, height = 8, dpi = 320)

set.seed(99)

n_birds <- 1e6
d <- 1800

# Generate base noise grid
noise_grid <- noise_perlin(dim = c(d, d))

# Pre-allocate vectors
base_x <- rnorm(n_birds)
base_y <- rnorm(n_birds)

scaled_x <- round(scales::rescale(base_x, to = c(1, d)))
scaled_y <- round(scales::rescale(base_y, to = c(1, d)))

# Get noise values (vectorized)
noise_x <- noise_grid[cbind(pmin(pmax(scaled_x, 1), d), 
                            pmin(pmax(scaled_y, 1), d))]
noise_y <- noise_grid[cbind(pmin(pmax(scaled_y, 1), d), 
                            pmin(pmax(scaled_x, 1), d))]

# Generate flock data
fd <- data.table(
  id = 1:n_birds,
  x = base_x + noise_x,
  y = base_y + noise_y,
  s = abs(noise_x * noise_y)^0.2
)

ggplot(fd) +
  geom_spoke(aes(scaled_x, scaled_y, color = s, radius = (s * 30)^2, angle = s*3), linewidth = 0.1, alpha = 0.1) +
  scale_color_gradientn(colors = c("grey9", "grey99")) +
  # scale_fill_gradientn(colors = c("grey9", "grey97")) +
  coord_fixed() +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey9", color = NA)
  )
  
