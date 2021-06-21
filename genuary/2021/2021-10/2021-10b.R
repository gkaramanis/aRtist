library(ape)
library(ggtree)
library(ggplot2)

trees <- lapply(rep(100, 9), rtree, rooted = TRUE)
class(trees) <- "multiPhylo"

# ggplot(tree) +
#   geom_tree(aes(ladderize=TRUE))

ggtree(trees, ladderize = TRUE, branch.length = "none", layout = "circular",
       size = 0.3, color = "#1e2761") +
  geom_tippoint(color = "#FDAC4F", size = 0.7) +
  # geom_highlight(node = round(runif(100, 1, 100)), fill = "purple", size = 1) +
  facet_wrap(vars(.id)) +
  theme_void() +
  theme(
    strip.text = element_blank(),
    plot.background = element_rect(fill = "grey97", color = NA)
  ) 

ggsave(here::here("genuary", "2021", "2021-10", "2021-10b.png"), dpi = 320, width = 7, height = 7)

