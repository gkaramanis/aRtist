library(tidyverse)
library(threed)

obj_list <- list()

b = 16

for (i in 1:b^2) {
  camera_to_world <- look_at_matrix(eye = c(3, 3, 4), at = c(0, 0, 0))
  
  obj <- mesh3dobj$cube %>%
    rotate_by(angle = rnorm(1, 0, pi/8), v = c(0, i %% 4, 0)) %>% 
    transform_by(invert_matrix(camera_to_world)) %>%
    orthographic_projection() %>% 
    as.data.frame() %>% 
    mutate(n = i - 1)
  
  obj_list[[i]] <- obj
}

objs <- obj_list %>% reduce(rbind)

ggplot(objs) +
  geom_polygon(aes(x + 4 * n %% b, y + 4 * n %/% b, group = interaction(n, zorder), fill =  0.45 * fnx + fny), color = "grey20") +
  scale_fill_distiller(palette = "YlGnBu") +
  coord_cartesian(expand = FALSE, clip = "off") +
  # facet_wrap(vars(n)) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#8c7d70", color = NA),
    plot.margin = margin(20, 20, 20, 20)
  ) 

ggsave(here::here("genuary", "2021", "2021-26", "2021-26.png"), dpi = 320, width = 6.555, height = 7)
               
