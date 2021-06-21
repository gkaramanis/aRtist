library(tidyverse)
library(ggforce)

n = 150

rows_df <- data.frame(y = 1:n) %>%
  rowwise() %>% 
  mutate(x = list(-n/2:n/2 + rnorm(n - 1, 0, sd = y^3))) %>%
  ungroup() %>% 
  unnest(x) %>% 
  arrange(y, x) %>% 
  group_by(y) %>% 
  mutate(xn = row_number()) %>% 
  ungroup()

ggplot(rows_df, aes(x, y^2, group = xn)) +
  geom_bspline(size = 0.3, n = 100, color = "#ff4d00") +
  geom_bspline(size = 0.15, n = 500, color = "#f9f9f9") +
  scale_y_reverse() +
  coord_cartesian(expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "grey15", color = NA),
    plot.margin = margin(0, 20, 0, 20)
    
  ) 

ggsave(here::here("genuary", "2021", "2021-19", "2021-19.png"), dpi = 320, width = 12, height = 15)

