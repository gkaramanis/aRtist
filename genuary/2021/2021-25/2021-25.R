library(tidyverse)

prmtns <- data.frame(
  a = 0:8,
  b = 0:8,
  c = 0:8
) %>% 
  expand(a, b, c) %>% 
  filter(a != b) %>% 
  filter(b != c) %>% 
  filter(a != c) %>%
	mutate(n = row_number())

pal <- c('#e6194B', '#3cb44b', '#ffe119', '#4363d8', '#f58231', '#42d4f4', '#f032e6', '#fabed4', '#469990')

ggplot(prmtns) +
  geom_tile(data = NULL, aes(1, 1, height = 3.1, width = 3.1), fill = "grey85", color = NA) +
  geom_tile(aes(a %% 3, a %/% 3, fill = factor(a)), color = NA) +
  geom_tile(aes(b %% 3, b %/% 3, fill = factor(b)), color = NA) +
  geom_tile(aes(c %% 3, c %/% 3, fill = factor(c)), color = NA) +
  scale_fill_manual(values = pal) +
  coord_fixed(expand = FALSE) +
	facet_wrap(vars(n), ncol = 18) +
  theme_void() +
	theme(
		strip.text = element_blank(),
		legend.position = "none",
		plot.background = element_rect(fill = "grey97", color = NA),
		plot.margin = margin(20, 20, 20, 20)
	) +
  ggsave(here::here("genuary", "2021", "2021-25", "2021-25.png"), dpi = 320, height = 7, width = 4.675)

