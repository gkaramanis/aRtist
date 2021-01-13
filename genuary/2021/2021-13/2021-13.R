library(ggplot2)
library(dplyr)
library(binaryLogic)
library(ggforce)

# https://stackoverflow.com/questions/3789968/generate-a-list-of-primes-up-to-a-certain-number

primest <- function(n){
  p <- 2:n
  i <- 1
  while (p[i] <= sqrt(n)) {
    p <-  p[p %% p[i] != 0 | p==p[i]]
    i <- i+1
  }
  p
}

nm = 30000

prime_df <- data.frame(x = primest(nm)) %>% 
  rowwise() %>% 
  mutate(
    bin = paste0(as.binary(x), collapse = ""),
    bin_rev = intToUtf8(rev(utf8ToInt(bin))),
    y = x - strtoi(bin_rev, base = 2)
    ) %>% 
  ungroup() %>% 
  mutate(n = row_number())

ggplot(prime_df) +
  geom_tile(aes(x, y, fill = factor(sign(y)), height = n/3, width = n/3), color = "brown", alpha = 0.8) +
  scale_fill_manual(values = c("#ffd55a", "#293250", "#6dd47e")) +
  # coord_fixed() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "grey57", color = NA),
    legend.position = "none"
  ) +
  ggsave(here::here("genuary", "2021", "2021-13", "2021-13.png"), dpi = 320, width = 7, height = 7)

