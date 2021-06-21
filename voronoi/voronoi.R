library(tidyverse)
library(ggforce)


voro <- data.frame(th = seq(0, 5 * pi, by = 0.25)) %>%
  mutate(
    r = -0.1 * 1.2 ^ th,
    x = r * cos(th),
    y = r * sin(th)
)


ggplot(voro, aes(x, y)) +
  geom_voronoi_tile(aes(fill = x %% y / r),
                    max.radius = 0.9,
                    expand = -0.005,
                    radius = 0.005
                    ) +
  # geom_voronoi_segment() +
  scale_fill_viridis_c(option = "plasma") +
  # geom_point() +
  # scale_x_continuous(limits = c(-2, 2)) +
  # scale_y_continuous(limits = c(-2, 2)) +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey10", color = NA)
    ) 

ggsave(here::here("voronoi", "plots", paste0("voronoi-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320,
         width = 7, height = 6.105)

