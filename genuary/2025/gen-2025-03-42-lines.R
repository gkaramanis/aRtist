library(tidyverse)
library(camcorder)

gg_record(here::here("genuary/genuary-temp/"), width = 8, height = 8, dpi = 320)

set.seed(99)

lines_df <- read_lines(here::here("genuary/2025/gen-2025-03-42-lines.R")) %>% 
  str_split("\n") %>% 
  unlist() %>% 
  as_tibble() %>% 
  mutate(
    l = row_number(),
    y = nrow(.) - l,
    empty_line = if_else(str_length(value) == 0, TRUE, FALSE),
    glitched_text = str_split(value, "") %>%
      map_chr(~{
        chars <- unlist(str_split(.x, ""))
        glitch_chars <- c("▯", "▮", "░", "▒", "▓")
        # For each character, randomly decide if it should be replaced
        replaced_chars <- sapply(chars, function(c) {
          if (runif(1) < 0.15 && c != " ") {  # 15% chance, preserve spaces
            sample(glitch_chars, 1)
          } else {
            c
          }
        })
        paste(replaced_chars, collapse = "")
      })
  )

ggplot(lines_df) +
  geom_text(aes(x = 0.2,  y = y, label = glitched_text),
            family = "mono", size = 3.5, color = "grey97", hjust = 0) +
  geom_text(aes(x = 0,  y = y, label = sprintf("%02d", l), color = empty_line),
            family = "mono", size = 4, hjust = 1) +
  scale_color_manual(values = c("grey97", "grey50")) +
  coord_cartesian(xlim = c(-0.2, 8), ylim = c(0, 42), expand = TRUE) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey9", color = NA),
    plot.margin = margin(10, 10, 10, 10)
  ) 