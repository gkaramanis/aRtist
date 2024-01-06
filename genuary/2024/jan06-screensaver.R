library(tidyverse)
library(ggfx)

set.seed(9)

# https:ggfittext# https://github.com/Rezmason/matrix
chars <- 'モエヤキオカ7ケサスz152ヨタワ4ネヌナ98ヒ0ホア3ウ セ¦:"꞊ミラリ╌ツテニハソ▪—<>0|+*コシマムメ'

n <- 300

rain <- data.frame(
  x = runif(n, 1, 10),
  y0 = runif(n, 1, 10)
) %>% 
  rowwise() %>%
  mutate(y = list(seq(y0, y0 + runif(1, 0, 5), 0.15))) %>% 
  ungroup() %>%
  unnest(y) %>% 
  group_by(y0) %>% 
  mutate(s = row_number()) %>% 
  ungroup() %>% 
  rowwise() %>% 
  mutate(
    r = runif(1, 1, nchar(chars)),
    ch = substr(chars, r, r)
    ) %>% 
  ungroup() 
  
  
f1 <- "Outfit"

ggplot(rain) +
  with_outer_glow(
  geom_text(aes(x, y, label = ch, alpha = rev(s), color = s == min(s)), family = f1, fontface = "bold", size = 3),
  colour = "grey40",
  sigma = 5,
  expand = 5
  ) +
  scale_alpha_continuous(range = c(-0.05, 1)) +
  scale_color_manual(values = c("grey60", "white")) +
  coord_fixed(xlim = c(1, 10), ylim = c(1, 10), expand = FALSE) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey15", color = NA)
  )
