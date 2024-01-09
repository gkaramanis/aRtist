library(ambient)
library(dplyr)
library(ggplot2)
library(camcorder)

gg_record(here::here("genuary/genuary-temp/"), width = 8, height = 8, dpi = 320)

# Character ramp from http://paulbourke.net/dataformats/asciiart/
ramp <- " .:-=+*#%@"

f1 <- "Outfit"            

i = 24 * pi

expand.grid(
  x = 1:i, 
  y = 1:i
) %>% 
  mutate(
    n = gen_waves(x, y * x, frequency = 0.04, seed = 99),
    ns = round(scales::rescale(n, from = c(min(n), max(n)), to = c(1, 10)))
  ) %>% 
  rowwise() %>% 
  mutate(c = substr(ramp, ns, ns)) %>% 
  ggplot() +
  geom_tile(aes(x, y, fill = n)) +
  geom_text(aes(x, y, label = c, color = n), size = 4, family = f1) +
  scale_fill_gradient2(low = "grey30", high = "grey40") +
  scale_color_gradient2(low = "black", high = "white") +
  coord_fixed() +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey15", color = NA)
  )

