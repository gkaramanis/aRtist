library(ggplot2)
library(randomcoloR)
library(here)

makeMatrix <- function(n) {
  z <- data.frame(matrix(ncol = n + 1, nrow = n ^ 2))
  
  z[1] <- rep(1:n, rep(n, n), length.out = n ^ 2)
  z[2] <- rep(1:n, rep(n, n))
  
  q <- "ggplot(z)"
  for(i in 1:(n-1)) {
    z[i+2] <- randomColor(count = n ^ 2, hue = c("red"))
    nam <- paste("X", i+2, sep = "")
    wh <- 1-(i-1)/n
    q <- paste(q, " + geom_tile(aes(X1, X2, width = ", wh, ", height = ", wh, ", fill = ", nam, "))", sep = "")
  }
  
  qq <- paste(q, "theme_void() + theme(legend.position = \'none\')", sep = " + ")
  print(qq)
  eval(parse(text = qq))
  }

makeMatrix(5)

ggsave(here("squares.png"), height = 5, width = 5)
