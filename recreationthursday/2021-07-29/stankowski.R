library(tidyverse)

# Code by Miles Ott!
# https://github.com/MilesOtt/recreation_thursday/blob/master/July_29_2021/first_go.Rmd

squares <- data.frame(
  lengths <- seq(1, .15, length = 21), # 1-.16
  xmin <- c(0, #1
             0, #2
             lengths[2], #3
             lengths[1], #4
             lengths[1], #5
             lengths[1]-lengths[6], #6
             lengths[1]-lengths[6]-lengths[7], #7
             0-lengths[8], #8
             0-lengths[9], #9
             0-lengths[10], #10
             0-lengths[11], #11
             0, #12
             lengths[12], #13
             sum(lengths[12:13]), #14
             sum(lengths[12:14]), #15
             sum(lengths[12:15]), #16
             rep(sum(lengths[2:3]), 5)), #17-21
  
  ymin <- c(0, #1
             -lengths[1], #2
             -lengths[1]-lengths[2]+lengths[3], #3
             -lengths[1]-lengths[2]+lengths[3]+lengths[4], #4
             -lengths[1]-lengths[2]+lengths[3]+lengths[4]+lengths[5], #5
             +lengths[6], #6
             +lengths[7], #7
             0, #8
             -lengths[8], #9 
             -sum(lengths[8:9]), #10
             -sum(lengths[8:10]), #11
             -rep(sum(lengths[1:2]), 5), #12-16 
             -sum(lengths[1:2])+lengths[17], #17
             -sum(lengths[1:2])+sum(lengths[17:18]), #18
             -sum(lengths[1:2])+sum(lengths[17:19]), #19
             -sum(lengths[1:2])+sum(lengths[17:20]), #20
             -sum(lengths[1:2])+sum(lengths[17:21])) #21
  ) %>% 
  mutate(
    xmax = xmin + lengths,
    ymax = ymin + lengths
  )

names(squares) <- c("lengths", "xmin", "ymin", "xmax", "ymax")

ggplot(squares) +
  geom_rect(aes(xmin = xmin,
                ymin = ymin,
                xmax = xmax,
                ymax = ymax), fill = NA, color = "grey20", size = 4) +
  coord_fixed() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#e5e5e3", color = NA)
  )


matter <- squares %>% 
  select(xmin, ymin, lengths) %>%
  mutate(
    xmin = xmin + lengths/2,
    ymin = ymin - lengths/2
    ) %>% 
  mutate(
    across(everything(), ~round(.x * 100)),
    xmin = xmin + 325,
    ymin = 120 - ymin,
    l = LETTERS[1:21]
  ) %>% 
  rowwise() %>% 
  mutate(code = paste0(
    "var box", l, " = Bodies.rectangle(", xmin, ", ", ymin, ", ", lengths, ", ", lengths, ", {
	render: {
		lineWidth: 12,
		fillStyle: 'transparent',
		strokeStyle: '#1a1b1d'
	}
});")) %>% 
  mutate(boxes = paste0("box", l)) %>% 
    ungroup()

# paste(matter$boxes, collapse = ", ")
writeLines(matter$code)
