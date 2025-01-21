library(tidyverse)
library(ggforce)
library(camcorder)

gg_record(dir = here::here("genuary/genuary-temp/"), width = 8, height = 8, dpi = 320)

set.seed(909)

n_particles <- 1400

particles <- tibble(
  id = 1:n_particles,
  x = runif(n_particles, 0.05, 0.95),
  y = runif(n_particles, 0.05, 0.95),
  r = c(runif(n_particles/5, 0.008, 0.02), rep(0.005, n_particles - n_particles/5))
)

collisions <- crossing(
  p1 = particles$id,
  p2 = particles$id
) %>%
  filter(p1 < p2) %>% 
  left_join(particles, by = c("p1" = "id")) %>%
  left_join(particles, by = c("p2" = "id"), suffix = c("1", "2")) %>%
  mutate(
    distance = sqrt((x1 - x2)^2 + (y1 - y2)^2),
    colliding = distance < (r1 + r2)
  ) %>%
  filter(colliding)

ggplot() +
  geom_circle(data = particles, aes(x0 = x, y0 = y, r = r), fill = "grey50", alpha = 0.5, color = NA) +
  geom_circle(data = collisions, aes(x0 = x2, y0 = y2, r = r2), fill = NA, color = NA) +
  geom_tile(data = collisions, aes(x = x2, y = y2, width = r2 * 2, height = r2 * 2), color = "grey80", fill = NA, alpha = 0.2, linewidth = 0.2) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey9", color = NA)
  )
    
