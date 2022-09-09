library(ggplot2)
library(ggforce)

r = seq(1, 11, 0.1)

ggplot() +
  geom_spiro(aes(r = r, R = r*20, d = r^2, outer = TRUE, color = r%%10), n = 2500, size = 3) +
  scale_color_viridis_c(option = "turbo") +
  coord_fixed() +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey97", color = NA)
  )
  
# Tweet ---

# library(ggplot2)
# library(ggforce)
# 
# r=seq(1,11,0.1)
# 
# ggplot()+geom_spiro(aes(r=r,R=r*20,d=r^2,outer=T,color=r%%10),size=3)+scale_color_viridis_c(option="turbo")+coord_fixed()+theme_void()+theme(legend.position="none")