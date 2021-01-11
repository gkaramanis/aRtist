library(tuneR)
library(dplyr)
library(ggplot2)
library(grid)

file_wav <- here::here("genuary", "2021", "2021-11", "data", "Beethoven_Piano_Concerto_4_slow_movement,_bars_47-55.wav")

# file_wav <- here::here("genuary", "2021", "2021-11", "data", "Beethoven,_Piano_Sonata,_Op_111,_first_movement_concluding_bars.wav")

wav <- readWave(file_wav)

wav_df <- data.frame(left = wav@left, right = wav@right) %>% 
  head(1500) %>%
  mutate(
    n = row_number(),
    g = n %/% (wav@samp.rate / 1000)
    ) 

p <- ggplot(wav_df, aes(right, left)) +
  geom_path(alpha = 0.2) +
  geom_point(size = 0.2, alpha = 0.1) +
  scale_x_continuous(limits = c(-7000, 7000)) +
  scale_y_continuous(limits = c(-7000, 7000)) +
  coord_fixed(expand = FALSE, clip = "off") +
  theme_minimal(base_family = "IBM Plex Mono") +
  theme(
    axis.title.x = element_text(angle = -90, vjust = 0.5),
    axis.text.x = element_text(angle = -90),
    axis.title.y = element_text(angle = 0, vjust = 0.5)
  )

png(file="genuary/2021/2021-11/2021-11b.png", width = 1200, height = 1200)
grid.newpage()
pushViewport(viewport(name = "rotate", angle = 45, width = 0.75, height = 0.75))
print(p, vp = "rotate")
dev.off()
