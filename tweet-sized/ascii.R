library(tidyverse)
library(ambient)

t <- seq(0,4,.01)

long_grid(x = t,
          y = t) %>% 
  mutate(
    n = gen_waves(x, y, t = tan(x*y) * sin(y^3)),
    n2 = (n - min(n)) / max(n - min(n)) * (10 - 1) + 1
  ) %>% 
  rowwise() %>% 
  mutate(l = substr(" .:-=+*#%@", n2, n2)) %>% 
  ggplot(aes(x, y, label = l)) +
  geom_text(size = 1) +
  coord_fixed() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "grey97", color = NA)
  )

# Tweet ---

# library(tidyverse)
# library(ambient)
# t=seq(0,4,.01)
# long_grid(x=t,y=t)%>%mutate(n=gen_waves(x,y,t=tan(x*y)*sin(y^3)),n2=(n-min(n))/max(n-min(n))*9+1)%>%rowwise()%>%mutate(l=substr(".:-=+*#%@",n2,n2))%>%ggplot(aes(x,y,label=l))+geom_text(size=1)+coord_fixed()+theme_void()
