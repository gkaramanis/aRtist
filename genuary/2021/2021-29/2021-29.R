library(tidyverse)
library(ggforce)

circles <- data.frame(
  x = runif(1, 0, 20),
  y = runif(1, 0, 20),
  r = rnorm(1, 0.02, 1),
  c = sample(1:5, 1)
)

i = 0

start_time <- Sys.time()

while (i < 2000) {
  new_line <- c(
    x = runif(1, 0, 20),
    y = runif(1, 0, 20),
    r = runif(1, 0.02, 1),
    c = sample(1:5, 1)
  )
	
  s = 0
  
  for (l in 1:nrow(circles)) {
    if (s == 0) {
    
    dd = sqrt((new_line[1] - circles$x[l])^2 + (new_line[2] - circles$y[l])^2)
    rr = new_line[3] + circles$r[l]
    
    s = sum(ifelse(dd < 1.1 * rr, 1, 0))
		#print(s)
     } else {break}
  }
  
  if (s == 0) {
    circles <- circles %>% 
      rbind(new_line)
    i = i + 1
    #print(i)
  }
}

print(Sys.time() - start_time)

pal <- c("#264653", "#2a9d8f", "#e9c46a", "#f4a261", "#e76f51")

ggplot(circles) +
  geom_circle(aes(x0 = x, y0 = y, r = r, fill = factor(c)), color = NA, n = 60) +
  scale_fill_manual(values = pal) +
  coord_fixed() +
  theme_void() +
  theme(
    legend.position = "none",
		plot.background = element_rect(fill = "grey95", color = NA)
  ) +
  ggsave(here::here("genuary", "2021", "2021-29", "2021-29.png"), dpi = 320, width = 7, height = 7)
