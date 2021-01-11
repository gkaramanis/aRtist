library(jsonlite)
library(janitor)
library(tidyverse)
library(ggfittext)

results_json <- fromJSON("http://openlibrary.org/search.json?subject=art")

results_sample <- results_json %>% 
  as.data.frame() %>% 
  clean_names() %>% 
  sample_n(60) %>% 
  mutate(
    n = row_number(),
    color = if_else(!is.na(docs_cover_i),
                   str_pad(paste0("#",
                             str_sub(docs_cover_i, end = -1)), 9, "right", pad = 0),
                   "grey50")
    ) %>% 
  rowwise() %>% 
  mutate(
    family = sample(c("Canela", "Graphik", "Graphik Light", "Produkt", "Produkt Medium", "Quotes Caps", "Courier", "Futura Medium"), 1),
    hjust = sample(c(1, 0, 0.5), 1),
    vjust = sample(c(1, 0, 0.5), 1)
    ) %>% 
  ungroup() %>% 
  select(n, docs_title, family, color, hjust, vjust)

ggplot(results_sample, aes(x = 0, y = 0, 
                      label = docs_title, family = family,
                      color = color, vjust = vjust, hjust = hjust)) +
  geom_tile(color = NA, fill = NA, width = Inf, height = Inf) +
  geom_fit_text(reflow = TRUE, grow = TRUE, fullheight = FALSE) +
  scale_color_identity() +
  # coord_fixed() +
  facet_wrap(vars(docs_title), ncol = 6) +
  theme_void() +
  theme(
    strip.text = element_blank(),
    panel.spacing = unit(0, "points")
    # plot.margin = margin(20, 20, 20, 20)
    ) +
  ggsave(here::here("genuary", "2021", "2021-12", "2021-12.png"), dpi = 320, height = 7, width = 9)
