library(grid)

pal <- c("#60C048", # green
         "#D83030", # red
         "#60A8D8", # blue
         "#F09048", # orange
         "#F0F048", # yellow
         "#7848A8", # purple
         "#202020") # grey

col = pal[rep(1:6, length.out = 32)]
lwd = 10

tree <- vpTree(
  viewport(x = 0.5, y = 0.5, height = 0.99, width = 0.99, name = "vp", "snpc"),
  vpList(
    viewport(x = 0.25, y = 0.25, height = 0.5, width = 0.5, clip = TRUE, name = "A"),
    viewport(x = 0.25, y = 0.75, height = 0.5, width = 0.5, clip = TRUE, name = "B"),
    viewport(x = 0.75, y = 0.5, height = 1, width = 0.5, clip = TRUE, name = "C")
  )
)

png(file = "recreationthursday/2021-week-03/plots/lewitt.png", width = 600, height = 600)

grid.newpage()
pushViewport(tree)

for (i in LETTERS[1:3]) {
  seekViewport(i)
  r = seq(2, 0.05, length.out = 50)
  if (i == "A") {
    x0 = 1
    y0 = 0
  } else if (i == "B") {
    x0 = 0
    y0 = 1
  } else {
    x0 = 0
    y0 = 0.5
  }
  circle <- circleGrob(x = x0, y = y0, r = r, gp = gpar(fill = col, col = NA))
  grid.draw(circle)
  grid.rect(gp = gpar(fill = NA, col = pal[7], lwd = lwd))
}

seekViewport("vp")
grid.rect(gp = gpar(fill = NA, col = pal[7], lwd = lwd))

dev.off()