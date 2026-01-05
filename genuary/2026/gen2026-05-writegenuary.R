library(tidyverse)
library(lofifonts)
library(camcorder)

gg_record(here::here("genuary/genuary-temp/"), width = 8, height = 8, dpi = 320)

i <- 80
light_x <- 40
light_y <- 15
n_steps <- 10

genuary <- bitmap_text_coords("genuary") |> 
  mutate(
    x = x + i/2 - floor(mean(range(x))),
    y = y + i/2 - floor(mean(range(y))),
    is_text = TRUE
  )

grid_base <- expand_grid(x = 1:i, y = 1:i) |> 
  mutate(
    dist_to_light = sqrt((x - light_x)^2 + (y - light_y)^2),
    ray_x = x - light_x,
    ray_y = y - light_y,
    ray_length = sqrt(ray_x^2 + ray_y^2)
  )

shadows <- grid_base |> 
  filter(ray_length >= 0.1) |> 
  crossing(genuary |> rename(letter_x = x, letter_y = y)) |> 
  mutate(
    to_x = letter_x - light_x,
    to_y = letter_y - light_y,
    dist = sqrt(to_x^2 + to_y^2),
    proj = (to_x * ray_x + to_y * ray_y) / ray_length,
    perp = sqrt((to_x - proj * ray_x / ray_length)^2 + 
                (to_y - proj * ray_y / ray_length)^2)
  ) |> 
  filter(dist < ray_length, dist > 0.1, perp < 0.8, proj > 0) |> 
  mutate(fade = pmax(0, 1 - (ray_length - dist) / 8)) |> 
  group_by(x, y) |> 
  summarise(shadow = max(fade), .groups = "drop")

solar_grid <- grid_base |> 
  left_join(shadows, by = c("x", "y")) |> 
  mutate(
    shadow = replace_na(shadow, 0),
    light = 1 / (1 + dist_to_light / 15),
    value = floor(pmax(0, light - shadow * 0.5) * n_steps) / n_steps
  )

ggplot(solar_grid, aes(x, y, fill = value)) +
  geom_tile() +
  geom_tile(data = genuary, fill = "white") +
  MetBrewer::scale_fill_met_c("Johnson") +
  coord_fixed(expand = FALSE) +
  theme_void() +
  theme(legend.position = "none")

record_polaroid()
