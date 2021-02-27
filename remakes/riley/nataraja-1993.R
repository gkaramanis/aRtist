library(tidyverse)

set.seed(1993)

nataraja <- expand.grid(c = 1:20, r = 0:24) %>% 
  mutate(
    short = if_else(row_number() %in% sample(1:500, 5), -0.3, 0)
    # long = if_else(row_number() %in% sample(1:500, 5), 0.3, 0)
  ) %>% 
  rowwise() %>% 
  mutate(
    x = list(c(c + short, c + 1.01, c + 1.01, c + short)),
    y = list(c(r + short, r + 1.01, r + 2.02, r + 1.01 + short)),
    col = round(runif(1, 1, 13), 0)
    ) %>% 
  ungroup() %>% 
  unnest(c(x, y))


pal <- c(
  "#C2644F", # dark orange
  "#A24D50", # brown
  "#DFA45B", # ochre
  "#E8C19A", # light ochre
  "#4167A5", # dark blue
  "#639ED5", # light blue
  "#ABC0E2", # very light blue
  "#3F6854", # dark green
  "#70884F", # green
  "#86BA84", # light green
  "#C8C866", # green-yellow
  "#BD81AC", # pink
  "#7B76B6" # purple
  )

ggplot(nataraja) +
  geom_polygon(aes(x = x, y = y, fill = factor(col), group = interaction(c, r)), color = NA) +
  scale_fill_manual(values = pal) +
  coord_cartesian(xlim = c(1, 21), ylim = c(1.5, 19.8), expand = FALSE) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.margin = margin(2, 2, 2, 2)
    ) +
  ggsave(here::here("remakes", "riley", "nataraja-1993.png"), dpi = 320, width = 6.9, height = 5)

