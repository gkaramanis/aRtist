library(tidyverse)
library(tuneR)
library(wesanderson)

# Read in midi files
vivaldi_au1 <- readMidi(here::here("genuary", "2021", "2021-28", "data", "autumn_no1_allegro_gp.mid"))

vivaldi_sp1 <- readMidi(here::here("genuary", "2021", "2021-28", "data", "spring_no1_allegro_gp.mid"))

# Get notes, convert time to angle and subtract pi/2, in order to start from the top of the plot
viv_notes_au1 <- vivaldi_au1 %>%
  getMidiNotes() %>% 
  mutate(
    t = time/max(time) * 2 * pi - pi / 2,
    concerto = "Autumn (Allegro)"
    )

viv_notes_sp1 <- vivaldi_sp1 %>%
  getMidiNotes() %>% 
  mutate(
    t = time/max(time) * 2 * pi - pi / 2,
    concerto = "Spring (Allegro)"
    )

# Merge 
viv_notes <- viv_notes_au1 %>% 
  bind_rows(viv_notes_sp1)

pal <- wes_palette("FantasticFox1", n = 16, type = "continuous")

ggplot(viv_notes) +
  geom_point(aes(x = note * cos(t), y = -note * sin(t), size = length, color = factor(track), alpha = velocity), stroke = 0) +
  scale_size_continuous(range = c(0, 18)) +
  scale_alpha_continuous(range = c(0.2, 0.9)) +
  scale_color_manual(values = rev(pal)) +
  coord_fixed(clip = "off") +
  facet_wrap(vars(concerto)) +
  theme_void(base_family = "Domaine Display Bold", base_size = 24) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey95", color = NA),
    plot.margin = margin(20, 10, 20, 10),
    panel.spacing = unit(0, "lines"),
    strip.text = element_text(margin = margin(0, 0, 5, 0))
  ) 

ggsave(here::here("genuary", "2021", "2021-28", "2021-28.png"), dpi = 320, width = 12, height = 6.418)
