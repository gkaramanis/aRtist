library(tidyverse)

grid_df <- data.frame(y = 1:40) %>% 
  mutate(x = list(1:11)) %>% 
  unnest(x) %>% 
  rowwise() %>% 
  mutate(
    x = list(c(x - 1, x, x + 1))
    ) %>% 
  unnest(x)

ggplot(grid_df) +
  geom_polygon(aes(x, y, group = y), fill = "black", color = "grey50") +
  coord_fixed(ratio = 0.25)
