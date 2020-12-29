library(tidyverse)
library(ambient)
library(here)
library(ggforce)

# Settings
nx = 30 # points on x axis
ny = 30 # points on y axis
curve_stroke = 0.1 # stroke width of lines
curve_alpha = 0.07 # alpha of lines

# Create matrix with nx * ny points, values are radius of shape
pnt <- array(noise_perlin(c(nx, ny)), dim = c(nx, ny), dimnames = list(1:nx, 1:ny)) * 10

pnt_df <- as.data.frame.table(pnt) %>% select("x" = Var1, "y" = Var2, "a" = Freq) %>%
  mutate_all(~ as.numeric(.)) %>% 
  mutate(a = abs(a))

pnt_splines <- pnt_df %>% 
  mutate(
  r = a*5,
  x2 = lapply(r/2, function(x)
  {y = rnorm(3, 1, r/5)
  x*c(-1, 1, 1, -1) + y}),
  y2 = lapply(r/2, function(x)
  {y = rnorm(3, 1, r/5)
  x*c(1, 1, -1, -1) + y})
) %>%
  unnest(cols = c(x2, y2)) %>% 
  mutate(x2 = x + x2,
         y2 = y + y2)

# Plot and save image
ggplot(pnt_splines) +
  geom_bspline_closed(aes(x2, y2, group = r, fill = r), colour = NA, size = curve_stroke, alpha = curve_alpha) +
  scale_fill_viridis_c(option = "plasma") +
  theme_void() +
  theme(
    legend.position = "none"
  ) +
  ggsave(here::here("flow", "plots", paste0("blob-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 7, height = 7)