library(tidyverse)
library(hershey)
library(ggforce)
library(wesanderson)

string_df <- create_string_df(text = "Courage!", font = 'cursive') %>% 
  mutate(i = 1)

for (i in 2:5) {
  string_i <- create_string_df(text = "Courage!", font = 'cursive')
  string_i$i <- i
  string_df <- string_df %>% 
    add_row(string_i)
}

strings <- string_df %>% 
  mutate(y = y + i * 30)

pal <- wes_palette("Zissou1", 10, type = "continuous")

ggplot(strings, aes(x, y)) +
  geom_voronoi_tile(aes(fill = factor((x - y) %% 9)), color = "grey20", size = 0.3) +
  geom_bspline(aes(group = interaction(i, char_idx, stroke), color = char_idx + i), size = 2) +
  scale_fill_manual(values = pal) +
  scale_color_viridis_c(option = "inferno") +
  # coord_fixed() +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey97", color = NA)
  ) +
  ggsave(here::here("genuary", "2021", "2021-31", "2021-31.png"), dpi = 320, width = 5, height = 7)

