library(tidyverse)
library(ambient)

dfn <- long_grid(seq(1, 10, length.out = 200),
                 seq(1, 10, length.out = 200)) %>% 
  mutate(
    n1 = gen_perlin(x, y),
    n2 = gen_perlin(x, y)
  )

ggplot(dfn) +
  geom_spoke(aes(y, -x, radius = n1 * 1.5, angle = 9 / n2, color = n1), size = 1) +
  scale_color_gradientn(colors = MetBrewer::met.brewer("Signac", direction = -1), na.value = "cadetblue3") +
  coord_fixed() +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey97", color = NA)
  )
  
