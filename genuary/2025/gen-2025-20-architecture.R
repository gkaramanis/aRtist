library(tidyverse)
library(ggforce)
library(camcorder)

gg_record(here::here("genuary/genuary-temp/"), width = 8, height = 8, dpi = 320)

set.seed(929)

create_house <- function(x_center, y_center, width, height) {
  tibble(
    x = x_center + width * c(-0.5, 0.5, 0.5, 0, -0.5),
    y = y_center + c(0, 0, height * 0.6, height, height * 0.6)
  )
}

houses_df <- expand_grid(
  row = 1:9,
  col = 1:9
) %>%
  mutate(
    invert = runif(n()) < 0.6,
    house_color = ifelse(invert, "grey97", "grey9"),
    tile_color = ifelse(invert, "grey9", "grey97"),
    x_center = (col - 0.5) / 9,
    y_center = (row - 0.5) / 9,
    width = runif(n(), 0.05, 0.08),
    height = runif(n(), 0.06, 0.1)
  ) %>%
  rowwise() %>%
  mutate(
    coords = list(create_house(x_center, y_center, width, height))
  ) %>%
  unnest(coords) 

ggplot(houses_df) +
  geom_tile(aes(x_center, y_center + 0.05, col, fill = tile_color)) +
  geom_polygon(aes(x, y, fill = house_color, group = interaction(row, col))) +
  scale_fill_identity() +
  coord_fixed(clip = "off") +
  theme_void() +
  theme(plot.background = element_rect(fill = "grey97", color = NA))