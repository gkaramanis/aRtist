library(tidyverse)
library(ambient)

disturbance = expand.grid(c = 1:25, r = 1:49) %>% 
  mutate(
    c = ifelse(r %% 2 == 0, c + 0.5, c),
    a = 180 * gen_cubic(c, r, frequency = 0.1, seed = 1964)
    ) %>% 
  filter(c <= 25)
  
ggplot(disturbance) +
  geom_text(aes(c, r, label = "O", angle = a), family = "Times", size = 5) +
  coord_fixed(ratio = 0.5, expand = TRUE) +
  theme_void() +
  ggsave(here::here("remakes", "riley", "disturbance-1993.png"), dpi = 320, width = 6, height = 6)

