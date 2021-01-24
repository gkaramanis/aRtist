library(threed)

camera_to_world <- look_at_matrix(eye = c(4, 4, 4), at = c(0, 0, 0))

obj <- mesh3dobj$cube

cube <- obj %>%
  transform_by(invert_matrix(camera_to_world)) %>%
  orthographic_projection()

cube_df <- as.data.frame(cube) %>%
  filter(!hidden) %>% 
  mutate(i = 1)


ggplot(cube_df, aes(x, y, group = interaction(i, zorder))) +
  geom_polygon(aes(fill = fnx + fny + fnz), colour = 'black', size = 0.2) +
  scale_fill_viridis_c() +
  theme_void() +
  theme(legend.position = 'none') +
  coord_equal()

