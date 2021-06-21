library(ggforce)

centroids <- read_tsv(here::here("genuary", "2021", "2021-20", "data", "country-centroids.csv"))

centroids_xy <- centroids %>%   
  rowwise() %>% 
  mutate(
    x = list(c(longitude/5, longitude, longitude/5)),
    y = list(c(-150, latitude, 150))
  ) %>% 
  unnest(c(x, y))

ggplot(centroids, aes(longitude, latitude)) +
  geom_delaunay_tile(fill = "pink") +
  geom_delaunay_segment(color = "black") +
  coord_fixed() +
  theme_void() +
  theme(legend.position = "none") 

ggsave(here::here("genuary", "2021", "2021-20", "2021-20.png"), dpi = 320)

