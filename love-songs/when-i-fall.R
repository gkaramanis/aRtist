library(tidyverse)
library(tidytext)
library(genius)
library(here)
library(ggpage)

# artist = "Nat King Cole"
# song = "When I fall in love"
# 
# song = "you give love a bad name"
# artist = "bon jovi"
# 
# song = "livin on a prayer"
# artist = "bon jovi"

song = "bad romance"
artist = "lady gaga"

lyrics <- genius_lyrics(
  artist = artist,
  song = song
  ) %>%
  select(line, text = lyric) %>%
  mutate(
    text = replace_na(text, ""),
    text = paste0(text, "\n")
    ) %>% 
  add_row(text = song, line = -1, .before = 1) 

lyrics %>% 
  ggpage_quick()

song_words <- lyrics %>% 
  unnest_tokens(word, text) %>% 
  mutate(
    chars = nchar(word),
    wordcolor = case_when(
      word == "love" ~ "orangered2",
      TRUE ~ "black"
    )
    ) %>% 
  mutate(wordstart = 1) %>% 
  group_by(line) %>% 
  mutate(
    wordstart = cumsum(lag(wordstart, default = 0) +
      nchar(lag(word, default = 0))),
    wordend = wordstart + nchar(word)
         )

ggplot(song_words) +
  geom_rect(aes(xmin = wordstart, ymin = line - 0.4,
            xmax = wordend, ymax = line + 0.4,
            fill = wordcolor), color = NA) +
  scale_fill_identity() +
  scale_y_reverse() +
  coord_fixed(ratio = 2) +
  theme_void() 

songfile <- str_replace_all(song, "[^[:alnum:]]", "-") %>%
  str_sub(., 1, 25) %>% tolower() %>%
  paste0(., ".png")

ggsave(here::here("images", songfile),
       height = 14, width = 10, dpi = 320)
  

