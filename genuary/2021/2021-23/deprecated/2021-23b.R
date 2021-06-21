plot = ggplot()  

for (i in 1:500) {
  plot + 
    geom_polygon(data = data.frame(x = pnts$x[i], y = pnts$y[i], idx = pnts$idx[i]) %>% 
                   rowwise() %>% 
                   mutate(
                     x = list(c(x + 1, x + 0.5, x - 0.5, x - 1, x - 0.5, x + 0.5)),
                     y = list(c(y, y + 0.866, y + 0.866, y, y - 0.866, y - 0.866))
                   ) %>% 
                   unnest(c(x, y)),
                 aes(x, y, fill = factor(idx %% 5), color = factor(idx %% 5)), alpha = 1, size = 0.4
              ) +
    geom_spoke(data = data.frame(x = pnts$x[i], y = pnts$y[i], idx = pnts$idx[i]), 
               aes(x, y, angle = 60 * pi / 180, radius = 1, color = factor(idx %% 5)), alpha = 1, size = 0.25) +
    geom_spoke(data = data.frame(x = pnts$x[i], y = pnts$y[i], idx = pnts$idx[i]), 
               aes(x, y, angle = pi, radius = 1, color = factor(idx %% 5)), alpha = 1, size = 0.25) +
    geom_spoke(data = data.frame(x = pnts$x[i], y = pnts$y[i], idx = pnts$idx[i]), 
               aes(x, y, angle = 300 * pi / 180, radius = 1, color = factor(idx %% 5)), alpha = 1, size = 0.25) -> plot
}

plot +
  scale_fill_manual(values = pal) +
  scale_color_manual(values = colorspace::darken(pal, 0.3)) +
  coord_fixed() +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey90", color = NA)
  ) 

ggsave(here::here("genuary", "2021", "2021-23", "2021-23b.png"), dpi = 320, width = 7, height = 7)
