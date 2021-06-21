library(tidyverse)
library(wesanderson)

n = 45

fun_df <- data.frame(y = 1:n) %>%
  rowwise() %>% 
  mutate(x = list(rnorm(n, mean = n/20, sd = n/5))) %>% 
  unnest(x)

pal <- wes_palette("FantasticFox1", 16, type = "continuous")

ggplot(fun_df) +
  geom_tile(aes(x * 4, y - x * c(1, -1), group = y, height = x, width = x, fill = factor(y %/% 3)), color = NA, alpha = 0.1) +
  scale_fill_manual(values = pal) +
  # coord_fixed(clip = "off") +
  theme_void() +
  xlim(-100, 100) +
  ylim(-20, 70) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey15", color = NA)
    ) 

ggsave(here::here("genuary", "2021", "2021-18", "2021-18.png"), dpi = 320, width = 10, height = 7)
  
