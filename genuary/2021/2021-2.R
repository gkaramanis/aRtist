library(tidyverse)
library(ggforce)

rule30 <- read_csv(here::here("genuary", "2021", "data", "rule30.csv"), col_names = FALSE)

rule30_xy <- rule30 %>% 
  mutate(y = row_number()) %>% 
  pivot_longer(`X1`:`X101`, names_to = "x") %>% 
  filter(value == 1) %>% 
  mutate(
    x = as.numeric(str_remove(x, "X")),
    color1 = x %% y,
    color2 = x %% 2
    )

ggplot(rule30_xy) +
  geom_voronoi_tile(aes(x, y, fill = color1), color = "grey20", size = 0.1) +
  scale_y_reverse() +
  scale_fill_viridis_c(option = "magma") +
  coord_cartesian(expand = FALSE) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey10", color = NA),
    plot.margin = margin(2, 2, 2, 2)
  ) +
  ggsave(here::here("genuary", "2021", "2021-2.png"), dpi = 320, width = 7, height = 3.7)

