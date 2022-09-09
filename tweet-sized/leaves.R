library(ggplot2)

e <- 1e-3
s <- 1e4
t <- pi/2 * cumsum(seq(e, -e, length.out = s))^3

ggplot() +
  geom_spoke(aes(
    x = cumsum(cos(t)),
    y = cumsum(sin(t)),
    angle = t,
    color = t,
    radius = 1:s %% 500
  ), alpha = 0.5) +
  scale_color_distiller(palette = 15, guide = "none") +
  coord_fixed() +
  theme_void()


# Tweet ---

# library(ggplot2)
# e=1e-3
# s=1e4
# t=pi/2*cumsum(seq(e,-e,length.out=s))^3
# 
# ggplot()+geom_spoke(aes(x=cumsum(cos(t)),y=cumsum(sin(t)),angle=t,color=t,radius=1:s%%500),alpha=0.5)+scale_color_distiller(palette=15,guide="none")+coord_fixed()+theme_void()
