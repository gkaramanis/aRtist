library(tidyverse)
library(sfheaders)

geom_building <- function(x, y, h) {
  building <- list(
  # building
  geom_polygon(aes(x_right, y_right, group = n), color = c, fill = fill_right),
    geom_polygon(aes(x_left, y_left, group = n), color = c, fill = fill_left),
    # roof
    geom_polygon(aes(roof_x, roof_y, group = n), color = c, fill = fill_roof)
  )
  
  return(building)
}


max_n = 600

buildings <- data.frame(
  n = 1:max_n,
  x = round(runif(max_n, 1, 20)),
  y = 0,
  h = c(round(runif(max_n/3, 20, 30)), round(runif(max_n/3, 10, 20)), round(runif(max_n/3, 0, 10)))
) %>% 
  rowwise() %>% 
  mutate(
    part = list(c("right", "left", "roof")),
    x = x + n %% 2 / 2,
    x = list(list(c(x, x, x + 0.5, x + 0.5),
                  c(x, x, x - 0.5, x - 0.5),
                  c(x - 0.5, x , x + 0.5, x))),
    y = list(list(c(y + c(0, h, h + 0.25, 0)),
                  c(y + c(0, h, h + 0.25, 0)),
                  c(y + c(h + 0.25, h + 0.5, h + 0.25, h))))
  ) %>% 
  ungroup() %>% 
  group_by(n) %>% 
  arrange(desc(y), .by_group = TRUE) %>% 
  ungroup() %>% 
  unnest(c(x, y, part)) %>% 
  unnest(c(x, y))

c = "black"
fill_right = "#EDB39F"
fill_left = "#655F7B"
fill_roof = "#B86F85"

pal_day = c("#EDB39F", "#655F7B", "#B86F85")

city <- buildings %>%
  sf_multipolygon(x = "x", y = "y", multipolygon_id = "n", polygon_id = "part") %>% 
  st_cast("POLYGON", group_or_split = TRUE) %>% 
  mutate(part = rep(c("right", "left", "roof"), length.out = length(n)))

ggplot(city) +
  geom_sf(aes(), alpha = 1) +
  scale_fill_manual(values = pal_day) +
  coord_sf(expand = FALSE) +
  theme_void() +
  theme(
    legend.position = "none"
  )

ggsave(here::here("cities", "plots", "city.png"), dpi = 320)
