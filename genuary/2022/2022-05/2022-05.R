library(tidyverse)
library(camcorder)

gg_record(dir = "genuary/2022/genuary-temp", device = "png", width = 10, height = 10, units = "in", dpi = 320)

set.seed(99)

sqs <- expand.grid(x = -5:5, y = -5:5) %>% 
  mutate(i = sample(c(TRUE, FALSE), nrow(.), replace = TRUE)) %>% 
  rowwise() %>% 
  mutate(
    xend = ifelse(i, x + sample(-15:15, 1), x),
    yend = ifelse(i, y + sample(-15:15, 1), y)
  )

ggplot(sqs) +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend), size = 0.2, color = "grey50") +
  geom_point(aes(x, y), shape = 21, fill = "grey97", color = "grey50") +
  geom_tile(aes(xend, yend, width = 0.75, height = 0.75, fill = ifelse(i, "maroon", "#266080")), color = NA) +
  scale_x_continuous(limits = c(-20, 20)) +
  scale_fill_identity() +
  coord_fixed() +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey97", color = NA)
  )
