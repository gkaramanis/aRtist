library(jsonlite)
library(janitor)
library(tidyverse)
library(ggfittext)

results_json <- fromJSON("http://openlibrary.org/search.json?subject=art")
# results_json <- fromJSON("http://openlibrary.org/search.json?subject=music")

n_max = 64

results_sample <- results_json %>% 
  as.data.frame() %>% 
  clean_names() %>% 
  filter(!is.na(docs_cover_i)) %>% 
  sample_n(n_max) %>% 
  mutate(
    n = row_number(),
    color = if_else(!is.na(docs_cover_i),
                   str_pad(paste0("#", str_sub(abs(docs_cover_i), end = -1)), 9, "right", pad = 0),
                   "grey50")
    ) %>% 
  rowwise() %>% 
  mutate(
    family = sample(c("Canela", "Graphik", "Graphik Light", "Produkt", "Produkt Medium", "Quotes Caps", "Courier", "Futura Medium", "Diamante Bold", "FinkHeavy Medium"), 1)
    ) %>% 
  ungroup() %>% 
  select(n, docs_title, family, color) %>% 
  mutate(
    x = rep(1:sqrt(n_max), sqrt(n_max)),
    y = rep(1:sqrt(n_max), each = sqrt(n_max))
    )

ggplot(results_sample, aes(x = x, y = y, 
                      label = str_wrap(docs_title, 10),
                      family = family, color = color)) +
  # geom_text(alpha = 1) +
  geom_tile(color = NA, fill = NA, width = Inf, height = Inf) +
  geom_fit_text(reflow = TRUE, grow = TRUE, fullheight = FALSE) +
  scale_color_identity() +
  # coord_fixed() +
  # facet_wrap(vars(docs_title), ncol = 6) +
  theme_void() +
  theme(
    legend.position = "none",
    strip.text = element_blank(),
    panel.spacing = unit(0, "points")
    ) +
  ggsave(here::here("genuary", "2021", "2021-12", "2021-12.png"), dpi = 320, height = 7, width = 9)
