library(treemapify)
library(ggplot2)

tree <- data.frame(n = 1:1900, v = runif(1900, 0, 4000), g = round(runif(1900, 1, 16)))

col1 <- "#e1dd72"
col2 <- "#1b6535"

ggplot(tree) +
  geom_treemap(aes(area = v, fill = v-n, subgroup = g), color = "grey10", start = "bottomright") +
  scale_fill_gradient(low = col2, high = col1) +
  coord_fixed(expand = FALSE) +
  theme(
    legend.position = "none"
        ) 

ggsave(here::here("genuary", "2021", "2021-10", "2021-10.png"), dpi = 320, width = 7, height = 7)
