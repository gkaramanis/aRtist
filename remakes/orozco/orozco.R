library(tidyverse)
library(ggforce)

pal <- c("#173a74", "#9c2024", "#d4b23d", "#e4e4e4")

set.seed(72)

# rows and columns
x = 1:7
y = 1:8

# Relationship of radius in original
# R + r = 1
# r = 0.735 * R
R = 0.576
r = 0.424

discs <- crossing(y, x) %>% 
  mutate(radius = rep(c(r, R), length.out = 56)) %>% 
  rowwise() %>% 
  mutate(
    color = pal[sample(c(1:4), 1)],
    start = sample(c(0, pi/2, pi, 3*pi/2), 1),
    # big disc -> half, small -> whole
    end =  if_else(radius == R, start + pi, start + 2*pi),
    # remove from edges
    radius = case_when(
      x == 1 & (start != 0 | radius == r) ~ 0,
      x == 7 & (start != pi | radius == r) ~ 0,
      TRUE ~ radius
    )
    ) %>% 
  ungroup() 

discs2 <- discs %>% 
  filter(radius > r & between(x, 2, 6)) %>% 
  sample_n(11) %>% 
  rowwise() %>% 
  mutate(
    start = start + pi,
    end = end + pi,
    color = sample(setdiff(pal, color), 1)
    ) %>% 
  ungroup()

discs_final <- bind_rows(discs, discs2)

ggplot(discs_final) +
  geom_arc_bar(aes(x0 = x, y0 = y, r0 = 0, r = radius, start = start, end = end, fill = color), color = NA) +
  scale_fill_identity() +
  xlim(0.4, 7.6) +
  ylim(0.4, 8.6) +
  # coord_fixed() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#D6D6D4", color = NA),
    plot.margin = margin(0, -50, 0, -50)
  ) +
  ggsave(here::here("remakes", "orozco", "plots", "orozco.png"), dpi = 320, height = 8, width = 5.65)
