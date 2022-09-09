library(ggforce)
library(tidyverse)

data.frame(t = seq(-6, 6, 0.003)) %>% 
  mutate(
    x = sin(5.05 * t)^2 * 2^cos(cos(5.3 * t)),
    y = sin(sin(5.05 * t)) * cos(9.9 * t)^2
  ) %>% 
  ggplot(aes(x, y)) +
  geom_voronoi_tile(aes(fill = x^y), color = "white", size = 0.3) +
  scale_fill_fermenter(guide = "none", palette = 7) +
  coord_flip() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "black")
  )
  
# Tweet ---

# library(ggforce)
# library(dplyr)
# tibble(t=seq(-6,6,8e-3))%>%mutate(x=sin(5*t)^2*2^cos(cos(8*t)),y=sin(sin(5*t))*cos(7*t)^2)%>%ggplot(aes(x,y,fill=sin(x^6)))+geom_voronoi_tile(color=1)+scale_fill_fermenter(guide="none",palette=7)+coord_flip()+theme_void()
