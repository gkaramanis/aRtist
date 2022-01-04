library(tidyverse)
library(ambient)
library(here)
library(MetBrewer)
library(camcorder)
library(ggforce)

set.seed(19)

gg_record(dir = "genuary/2022/genuary-temp", device = "png", width = 10, height = 10, units = "in", dpi = 320)

# Settings
nx = 200 # points on x axis
ny = 200 # points on y axis
n_curves = 5000 # number of lines to draw
n_steps = 9 # number of steps in every drawn line
step_length = 1 # length of step
limit = n_steps * step_length # limits when calculating lines
curve_stroke = 0.3 # stroke width of lines
curve_alpha = 1 # alpha of lines

# Create matrix with nx * ny points, values are angle with Perlin noise
pnt <- array(noise_perlin(c(nx, ny)), dim = c(nx, ny), dimnames = list(1:nx, 1:ny)) * pi * 2

# Create matrix for lines (segments with start and end points)
dat <- matrix(nrow = n_curves * n_steps, ncol = 7) 
colnames(dat) <- c("x_start", "y_start", "a", "x_end", "y_end", "l", "i")

# Loop for calculations
for (l in 1:n_curves-1) { # loop through lines
  # random start points within limits
  x_start = runif(1, min = limit, max = nx - (limit))
  y_start = runif(1, min = limit, max = ny - (limit))
  # get angle from the original matrix
  a = pnt[x_start, y_start]
  
  for (i in 1:n_steps) { # loop through steps in every line
    # calculate end point for first step
    x_end = x_start + step_length * cos(a)
    y_end = y_start + step_length * sin(a)
    # write first row of points and angle
    dat[i + l * n_steps, 1] = x_start
    dat[i + l * n_steps, 2] = y_start
    dat[i + l * n_steps, 3] = a
    dat[i + l * n_steps, 4] = x_end
    dat[i + l * n_steps, 5] = y_end
    dat[i + l * n_steps, 6] = l   # can use this for colour
    dat[i + l * n_steps, 7] = i   # can use this for colour
    # create starting point for next step from previous end point, get angle for the "new" point from the original matrix 
    x_start = x_end
    y_start = y_end
    a = pnt[x_start, y_start]
  }
}

# Convert matrix to data frame
dat_df <- as.data.frame(dat)

# Plot
pal <- met.brewer("Hiroshige")

ggplot(dat_df) +
  geom_delaunay_segment(aes(x = x_start, y = y_start,
                   xend = x_end, yend = y_end, color = l),
               size = curve_stroke, alpha = curve_alpha,
               lineend = "round", linejoin = "round") + # change those for different effects: lineend = c('round', 'butt', 'square'), linejoin = c('round', 'mitre', 'bevel')
  scale_color_gradientn(colours = pal) +
  coord_fixed(xlim = c(limit * 1.5, nx - limit * 1.5), ylim = c(limit * 1.5, ny - limit * 1.5), expand = FALSE) + # "crop" to fill the frame
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey20")
  ) 

